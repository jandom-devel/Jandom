/**
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 *
 * (c) 2011 Gianluca Amato
 */

package it.unich.sci.jandom
package domains

import it.unich.sci.jandom.parameters.ParameterValue

/**
 * This is the box abstract domain over doubles.
 *
 * It is actually a proof of concept on how to implement an abstract domain entirely in Scala. It
 * is not safe to use, since it does not implement rounding correctly. Real domains should be implemented
 * within $PPL or $APRON.
 *
 * @param low the lower bounds for the box
 * @param high the upper bounds for the box
 * @author Gianluca Amato <amato@sci.unich.it>
 */

final class BoxDouble(private val low: Array[Double], private val high: Array[Double]) extends NumericalProperty[BoxDouble] {
  require(normalized, "The parameters low:" + low.mkString(",") + " and high: " + high.mkString(",") + " are not normalized")

  /**
   * This checks whether the box is normalized. This should always be the case. A box is normalized when
   * the lower and higher bounds are of the same length, and either
   *   1. there are no lower bounds equal to +Inf,
   *     there are no upper bounds equal to -Inf, the lower bounds are smaller of the corresponding upper bounds or
   *   2. all the lower bound are +Inf and all the upper bounds are -Inf.
   * @return whether the box is normalized.
   */
  private def normalized: Boolean =
    low.length == high.length &&
      (
        low.forall { (x) => !(x.isPosInfinity) } &&
        high.forall { (x) => !(x.isNegInfinity) } &&
        (low, high).zipped.forall(_ <= _)
        ||
        low.forall { _.isPosInfinity } &&
        high.forall { _.isNegInfinity })

  /**
   * Returns the sum of x and y, rounded towards +Inf.
   * @note This is not implemented correctly.
   * @param x first number to sum.
   * @param y second number to sum.
   * @returns x+y rounded towards +Inf.
   */
  private def add_hi(x: Double, y: Double): Double = x + y

  /**
   * Returns the sum of x and y, rounded towards -Inf.
   * @note This is not implemented correctly.
   * @param x first number to sum.
   * @param y second number to sum.
   * @returns x+y rounded towards -Inf
   */
  private def add_lo(x: Double, y: Double): Double = x + y

  /**
   * Returns the product of x and y, rounded towards +Inf. Moreover,
   * if x is 0, the product is 0 independently from the value of y.
   * @note This is not implemented correctly.
   * @param x first number to multiply.
   * @param y second number to multiply.
   * @returns x*y rounded towards +Inf
   */
  private def mul_hi(x: Double, y: Double): Double = if (x == 0) 0 else x * y

  /**
   * Returns the product of x and y, rounded towards -Inf. Moreover, if x is 0,
   * the product is 0 independently from the value of y.
   * @note This is not implemented correctly.
   * @param x first number to multiply.
   * @param y second number to multiply.
   * @returns x*y rounded towards -Inf.
   */
  private def mul_lo(x: Double, y: Double): Double = if (x == 0) 0 else x * y

  /**
   * Return the dot product of x and y, rounded towards -Inf.
   * @note This is not implemented correctly.
   */
  private def dotprod_lo(x: Array[Double], y: Array[Double], remove: Int = -1): Double = {
    var sum: Double = 0
    for (i <- x.indices) if (i != remove) sum = add_lo(sum, mul_lo(x(i), y(i)))
    return sum;
  }

  /**
   * Return the dot product of x and y, rounded towards +Inf.
   * @note This is not implemented correctly.
   */
  private def dotprod_hi(x: Array[Double], y: Array[Double], remove: Int = -1): Double = {
    var sum: Double = 0
    for (i <- x.indices) if (i != remove) sum = add_hi(sum, mul_hi(x(i), y(i)))
    return sum;
  }

  def union(that: BoxDouble): BoxDouble = {
    require(dimension == that.dimension)
    val newlow = (this.low, that.low).zipped.map(_ min _)
    val newhigh = (this.high, that.high).zipped.map(_ max _)
    new BoxDouble(newlow, newhigh)
  }

  def intersection(that: BoxDouble): BoxDouble = {
    require(dimension == that.dimension)
    val newlow = (this.low, that.low).zipped.map(_ max _)
    val newhigh = (this.high, that.high).zipped.map(_ min _)
    new BoxDouble(newlow, newhigh)
  }

  /**
   * This is the standard widening on boxes based on [[http://www.di.ens.fr/~cousot/COUSOTpapers/ISOP76.shtml CC76]].
   */
  def widening(that: BoxDouble) = {
    val newlow = (low, that.low).zipped.map((l1, l2) => if (l1 == Double.PositiveInfinity) l2 else if (l1 <= l2) l1 else Double.NegativeInfinity)
    val newhigh = (high, that.high).zipped.map((l1, l2) => if (l1 == Double.NegativeInfinity) l2 else if (l1 >= l2) l1 else Double.PositiveInfinity)
    new BoxDouble(newlow, newhigh)
  }

  /**
   * This is the standard narrowing on boxes based on [[http://www.di.ens.fr/~cousot/COUSOTpapers/ISOP76.shtml CC76]].
   */
  def narrowing(that: BoxDouble) = {
    require(dimension == that.dimension)
    val newlow = (low, that.low).zipped.map((l1, l2) => if (l1 == Double.NegativeInfinity) l2 else l1 min l2)
    val newhigh = (high, that.high).zipped.map((l1, l2) => if (l1 == Double.PositiveInfinity) l2 else l1 max l2)
    new BoxDouble(newlow, newhigh)
  }

  /**
   * Compute the minimum and maximum value of a linear form in a box.
   * @note should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients.
   * @param known the in-homogeneous coefficient.
   * @return a tuple with two components: the first component is the least value, the second component is the greatest value
   * of the linear form over the box.
   */
  private def linearEvaluation(coeff: Array[Double], known: Double): Tuple2[Double, Double] = {
    require(coeff.length <= dimension)
    var newlow: Double = known
    var newhigh: Double = known
    for (i <- 0 to coeff.length - 1) {
      if (coeff(i) < 0) {
        newlow = add_lo(newlow, mul_lo(coeff(i), high(i)))
        newhigh = add_hi(newhigh, mul_hi(coeff(i), low(i)))
      } else {
        newlow = add_lo(newlow, mul_lo(coeff(i), low(i)))
        newhigh = add_hi(newhigh, mul_hi(coeff(i), high(i)))
      }
    }
    (newlow, newhigh)
  }

  /**
   * Compute the corner of the box which minimizes a linear form. We do not need the in-homogenous coefficients since it is not
   * relevant for the computation.
   * @note should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients.
   * @return the coordinates of the point which minimizes the linear form.
   */
  private def linearArgmin(coeff: Array[Double]): Array[Double] = {
    require(coeff.length <= dimension)
    (coeff.zipWithIndex) map { case (c, i) => if (c > 0) low(i) else high(i) }
  }

  /**
   * Compute the corner of the box which maximizes a linear form. We do not need the inhomogenous coefficients since it is not
   * relevant for the computation.
   * @note should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @return the coordinates of the point which maximizes the linear form
   */
  private def linearArgmax(coeff: Array[Double]): Array[Double] = {
    require(coeff.length <= dimension)
    (coeff.zipWithIndex) map { case (c, i) => if (c < 0) low(i) else high(i) }
  }

  /**
   * Linear assignment over a box.
   * @return the least box which contains the result of the linear assignment.
   * @throws IllegalDomainException if parameters are not correct.
   */
  def linearAssignment(n: Int, coeff: Array[Double], known: Double): BoxDouble = {
    require(n <= low.length && coeff.length <= dimension)
    if (isEmpty) return this
    val interval = linearEvaluation(coeff, known)
    new BoxDouble(low.updated(n, interval._1), high.updated(n, interval._2))
  }

  /**
   * @return the least box which contains the intersection with the half-plane `coeff*v+knwown <= 0`.
   * @throws IllegalDomainException if parameters are not correct.
   */
  def linearInequality(coeff: Array[Double], known: Double): BoxDouble = {
    require(coeff.length <= dimension)

    /* if the box is empty the result is empty */
    if (isEmpty) return this

    /* check if result is empty */
    val lfArgmin = linearArgmin(coeff)
    val lfMin = linearEvaluation(coeff, known)._1
    if (lfMin > 0) return BoxDouble.empty(dimension)

    val newlow = low.clone
    val newhigh = high.clone

    val infinities = (0 to (coeff.length - 1)) filter { i => lfArgmin(i).isInfinity && coeff(i) != 0 }
    infinities.size match {
      case 0 =>
        for (i <- 0 to (coeff.length - 1)) {
          if (coeff(i) < 0) newlow(i) = low(i) max (lfArgmin(i) - lfMin / coeff(i))
          if (coeff(i) > 0) newhigh(i) = high(i) min (lfArgmin(i) - lfMin / coeff(i))
        }
      case 1 => {
        val posinf = infinities.head
        if (coeff(posinf) < 0)
          newlow(posinf) = low(posinf) max ((-dotprod_lo(coeff, lfArgmin, posinf) - known) / coeff(posinf))
        else
          newhigh(posinf) = high(posinf) min ((-dotprod_hi(coeff, lfArgmin, posinf) - known) / coeff(posinf))
      }
      case _ =>
    }
    new BoxDouble(newlow, newhigh)
  }

  /**
   * @return the least box which contains the intersection with the complement of the line `coeff*v+knwown==0`.
   * @throws IllegalDomainException if parameters are not correct.
   */
  def linearDisequality(coeff: Array[Double], known: Double): BoxDouble =
    throw new IllegalAccessException("Unimplemented feature");

  def mkString(vars: IndexedSeq[String]): Seq[String] = {
    if (isEmpty)
      Seq("[void]")
    else
      for (i <- 0 until dimension) yield low(i) + " <= " + vars(i) + " <= " + high(i)
  }

  val dimension: Int = low.length

  def isEmpty: Boolean = (low, high).zipped.exists(_ > _)

  def isFull: Boolean = low.forall(_.isNegInfinity) && high.forall(_.isPosInfinity)

  def empty = BoxDouble.empty(low.length)

  def full = BoxDouble.empty(low.length)

  def tryCompareTo[B >: BoxDouble](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: BoxDouble =>
      if (this.equals(other))
        Some(0)
      else if ((this.low, other.low).zipped.forall(_ <= _) && (this.high, other.high).zipped.forall(_ >= _))
        Some(1)
      else if ((this.low, other.low).zipped.forall(_ >= _) && (this.high, other.high).zipped.forall(_ <= _))
        Some(-1)
      else
        None
    case _ => None
  }

  override def equals(other: Any): Boolean = other match {
    case other: BoxDouble => java.util.Arrays.equals(this.low, other.low) && java.util.Arrays.equals(this.high, other.high)
    case _ => false
  }

  override def hashCode: Int = 41 * (41 + low.hashCode) + high.hashCode
}

/**
 * The factory object for `BoxDouble` properties. It caches the value of `empty(n)` for
 * different `n`, so that we do not create multiple copies of them. This works since
 * numerical properties should be implemented to be immutables.
 *
 * The caching should be probably implemented in [[it.unich.sci.jandom.domains.NumericalDomain]], and
 * should be extended to both full and empty values.
 */
object BoxDouble extends NumericalDomain[BoxDouble] with ParameterValue {

  val name = "Box Double"

  val description = 
    "This is a native Scala implementation of boxes. It is not safe" +
    "and should not be used."

  /**
   * This is a cache for empty boxes.
   */
  private var cacheEmpty: Map[Int, BoxDouble] = Map()

  /**
   * Returns a box with given bounds. The box is normalized: if it is empty, it is replaced by a well-behaved empty box
   * generated by the method `empty`.
   * @param low lower bounds.
   * @param high upper bounds.
   * @return the normalized box with the specified bounds.
   * @throws IllegalArgumentExpection if the lengths of low and high are not the same.
   */
  def apply(low: Array[Double], high: Array[Double]): BoxDouble = {
    require(low.length == high.length)
    if (unnormalizedIsEmpty(low, high))
      empty(low.length)
    else
      new BoxDouble(low, high)
  }

  /**
   * Returns a box consisting of the single point `poDouble`.
   */
  def apply(poDouble: Array[Double]): BoxDouble = apply(poDouble, poDouble)

  def full(n: Int): BoxDouble = {
    new BoxDouble(Array.fill(n)(Double.NegativeInfinity), Array.fill(n)(Double.PositiveInfinity))
  }

  def empty(n: Int): BoxDouble = {
    if (!(cacheEmpty isDefinedAt n))
      cacheEmpty += (n -> new BoxDouble(Array.fill(n)(Double.PositiveInfinity), Array.fill(n)(Double.NegativeInfinity)))
    cacheEmpty(n)
  }

  /**
   * Check whether the possibly unnormalized box with bounds `low` and `high` is empty.
   * @return true if the box is empty.
   */
  private def unnormalizedIsEmpty(low: Array[Double], high: Array[Double]) = (low, high).zipped.exists(_ > _)
}

