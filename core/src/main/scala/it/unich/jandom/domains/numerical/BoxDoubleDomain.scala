/**
 * Copyright 2013 Gianluca Amato
 *
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
 */

package it.unich.jandom.domains.numerical

import it.unich.jandom.domains.CachedTopBottom

/**
 * This is the domain of boxes, also known as the interval domain. Bounds are represented by doubles. It may be
 * instantiated to be safe w.r.t. double arithmetics or real arithmetics. However, correctness w.r.t. real
 * arithmetics is obtained by artificially enlarging bounds of computations, hence results are not as precise
 * as possible with other implementations.
 *
 * This domain is implemented natively in Scala and is a sort of reference implementation for numerical domains.
 * Real domains should be better implemented within $PPL or $APRON.
 * @author Gianluca Amato <gamato@unich.it>
 *
 * @constructor Builds a box domain using doubles as bounds
 * @param overReals is true if the domain is correct w.r.t. real arithmetic, otherwise it is correct w.r.t.
 * double arithmetics.
 */
class BoxDoubleDomain(val overReals: Boolean) extends NumericalDomain {
  /**
   * This is the class representing a single box.
   *
   * @constructor Creates a box with given lower and upper bounds.
   * @param low the lower bounds of the box.
   * @param high the upper bounds of the box.
   * @param isEmpty is true when the box is empty. It is needed for the case of 0-dimensional boxes.
   * @note `low`, `high` and `isEmpty` should be normalized according to the method `normalized`
   * @throws IllegalArgumentException if parameters are not correct.
   */

  final class Property(val low: Array[Double], val high: Array[Double], val isEmpty: Boolean) extends NumericalProperty[Property] {
    require(normalized, s"The parameters low: ${low.mkString(",")}, high: ${high.mkString(",")} and isEmpty: ${isEmpty} are not normalized")

    type Domain = BoxDoubleDomain

    def domain = BoxDoubleDomain.this

    /**
     * This checks whether the box is normalized. This should always be the case. A box is normalized when
     * the lower and higher bounds are of the same length, and either
     *   1. there are no lower bounds equal to +Inf, there are no upper bounds equal to -Inf,
     *       the lower bounds are smaller of the corresponding upper bounds, isEmpty is false, or
     *   2. all the lower bound are +Inf and all the upper bounds are -Inf, isEmpty is true.
     * @return whether the box is normalized.
     */
    private def normalized: Boolean =
      low.length == high.length &&
        (
          low.forall { (x) => !(x.isPosInfinity) } &&
          high.forall { (x) => !(x.isNegInfinity) } &&
          (low, high).zipped.forall(_ <= _) &&
          !isEmpty
          ||
          low.forall { _.isPosInfinity } &&
          high.forall { _.isNegInfinity } &&
          isEmpty)

    /**
     * This computes the smallest y > x which is representable as a Double. Note that
     * x should not be -Infinity.
     */
    private def nextfp(x: Double): Double = {
      require(!x.isNegInfinity)
      if (x == 0)
        java.lang.Double.longBitsToDouble(1);
      else if (!x.isPosInfinity) {
        val xx: Long = java.lang.Double.doubleToLongBits(x);
        java.lang.Double.longBitsToDouble(
          if (x > 0) xx + 1
          else if (x == 0) 1
          else xx - 1)
      } else
        x
    }

    /**
     * This computes the largest  y < x which is representable as a Double.
     */
    private def prevfp(x: Double): Double = {
      if (x == 0)
        -nextfp(0.0)
      else
        -nextfp(-x)
    }

    /**
     * Returns the sum of `x` and `y`, rounded towards +Inf.
     */
    private def add_hi(x: Double, y: Double): Double = if (overReals && x != 0 && y != 0) nextfp(x + y) else x + y

    /**
     * Returns the sum of `x` and `y`, rounded towards -Inf.
     */
    private def add_lo(x: Double, y: Double): Double = if (overReals && x != 0 && y != 0) prevfp(x + y) else x + y

    /**
     * Returns the product of `x` and `y`, rounded towards +Inf.
     */
    private def mul_hi(x: Double, y: Double): Double = if (overReals && x != 0 && x != 1 && y != 0 && y != 1) nextfp(x * y) else x * y

    /**
     * Returns the product of `x` and `y`, rounded towards -Inf.
     */
    private def mul_lo(x: Double, y: Double): Double = if (overReals && x != 0 && x != 1 && y != 0 && y != 1) prevfp(x * y) else x * y

    /**
     * Return the dot product of `x` and `y`, rounded towards `+Inf`.
     * If element `x(i)` is zero, then `x(i)*y(i)` is `0` independently from the value of `y(i)`.
     * If `remove` is a valid index in `x` and `y`, the factor `x(remove) * y(remove)` is
     * removed from the dot product.
     */
    private def dotprod_lo(x: Seq[Double], y: Seq[Double], remove: Int = -1): Double = {
      var sum: Double = 0
      for (i <- x.indices if i != remove if x(i) != 0) sum = add_lo(sum, mul_lo(x(i), y(i)))
      sum
    }

    /**
     * Return the dot product of `x` and `y`, rounded towards `-Inf`.
     * If element `x(i)` is zero, then `x(i)*y(i)` is `0` independently from the value of `y(i)`.
     * If `remove` is a valid index in `x` and `y`, the factor `x(remove) * y(remove)` is
     * removed from the dot product.
     */
    private def dotprod_hi(x: Seq[Double], y: Seq[Double], remove: Int = -1): Double = {
      var sum: Double = 0
      for (i <- x.indices if i != remove if x(i) != 0) sum = add_hi(sum, mul_hi(x(i), y(i)))
      sum
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def union(that: Property): Property = {
      require(dimension == that.dimension)
      val newlow = (this.low, that.low).zipped.map(_ min _)
      val newhigh = (this.high, that.high).zipped.map(_ max _)
      new Property(newlow, newhigh, isEmpty && that.isEmpty)
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def intersection(that: Property): Property = {
      require(dimension == that.dimension)
      val newlow = (this.low, that.low).zipped.map(_ max _)
      val newhigh = (this.high, that.high).zipped.map(_ min _)
      BoxDoubleDomain.this(newlow, newhigh)
    }

    /**
     * This is the standard widening on boxes based on [[http://www.di.ens.fr/~cousot/COUSOTpapers/ISOP76.shtml CC76]].
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def widening(that: Property) = {
      require(dimension == that.dimension)
      val newlow = (low, that.low).zipped.map((l1, l2) => if (l1 == Double.PositiveInfinity) l2 else if (l1 <= l2) l1 else Double.NegativeInfinity)
      val newhigh = (high, that.high).zipped.map((l1, l2) => if (l1 == Double.NegativeInfinity) l2 else if (l1 >= l2) l1 else Double.PositiveInfinity)
      new Property(newlow, newhigh, isEmpty && that.isEmpty)
    }

    /**
     * This is the standard narrowing on boxes based on [[http://www.di.ens.fr/~cousot/COUSOTpapers/ISOP76.shtml CC76]].
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def narrowing(that: Property) = {
      require(dimension == that.dimension)
      if (that.isEmpty) {
        that
      } else {
        val newlow = (low, that.low).zipped.map((l1, l2) => if (l1 == Double.NegativeInfinity) l2 else l1 min l2)
        val newhigh = (high, that.high).zipped.map((l1, l2) => if (l1 == Double.PositiveInfinity) l2 else l1 max l2)
        BoxDoubleDomain.this(newlow, newhigh)
      }
    }

    /**
     * Compute the minimum and maximum value of a linear form in a box.
     * @param lf a linear form.
     * @return a tuple with two components: the first component is the least value, the second component is the greatest value
     * of the linear form over the box.
     */
    def linearEvaluation(lf: LinearForm[Double]): (Double, Double) = {
      require(lf.dimension <= dimension)
      var newlow: Double = lf.known
      var newhigh: Double = lf.known
      val coeffs = lf.homcoeffs
      if (isEmpty && coeffs.exists { _ != 0 })
        (Double.PositiveInfinity, Double.NegativeInfinity)
      else {
        for (i <- coeffs.indices) {
          if (coeffs(i) < 0) {
            newlow = add_lo(newlow, mul_lo(coeffs(i), high(i)))
            newhigh = add_hi(newhigh, mul_hi(coeffs(i), low(i)))
          } else if (coeffs(i) > 0) {
            newlow = add_lo(newlow, mul_lo(coeffs(i), low(i)))
            newhigh = add_hi(newhigh, mul_hi(coeffs(i), high(i)))
          }
        }
        (newlow, newhigh)
      }
    }

    def minimize(lf: LinearForm[Double]) = linearEvaluation(lf)._1

    def maximize(lf: LinearForm[Double]) = linearEvaluation(lf)._2

    def frequency(lf: LinearForm[Double]) = {
      val (min, max) = linearEvaluation(lf)
      if (min == max) Some(min) else None
    }

    /**
     * Compute the corner of the box which minimizes a linear form.
     * todo should be generalized to linear forms over arbitrary types.
     * @param coeff the homogeneous coefficients.
     * @return the coordinates of the point which minimizes the linear form.
     */
    private def linearArgmin(lf: LinearForm[Double]): Seq[Double] = {
      require(lf.dimension <= dimension)
      (lf.homcoeffs.zipWithIndex) map { case (c, i) => if (c > 0) low(i) else high(i) }
    }

    /**
     * Compute the corner of the box which maximizes a linear form.
     * @todo should be generalized to linear forms over arbitrary types.
     * @param coeff the homogeneous coefficients
     * @return the coordinates of the point which maximizes the linear form
     */
    private def linearArgmax(lf: LinearForm[Double]): Seq[Double] = {
      require(lf.dimension <= dimension)
      (lf.homcoeffs.zipWithIndex) map { case (c, i) => if (c < 0) low(i) else high(i) }
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def nonDeterministicAssignment(n: Int): Property = {
      require(n < low.length && n >= 0)
      if (isEmpty)
        this
      else
        new Property(low.updated(n, Double.NegativeInfinity), high.updated(n, Double.PositiveInfinity), false)
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @todo @inheritdoc
     * @throws $ILLEGAL
     */
    def linearAssignment(n: Int, lf: LinearForm[Double]): Property = {
      require(n < low.length && n >= 0 && lf.dimension <= dimension)
      if (isEmpty)
        this
      else {
        val interval = linearEvaluation(lf)
        new Property(low.updated(n, interval._1), high.updated(n, interval._2), false)
      }

    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @todo @inheritdoc
     * @throws $ILLEGAL
     */
    def linearInequality(lf: LinearForm[Double]): Property = {
      require(lf.dimension <= dimension)

      /* if the box is empty the result is empty */
      if (isEmpty) return this

      /* check if result is empty */
      val lfArgmin = linearArgmin(lf)
      val lfMin = linearEvaluation(lf)._1
      if (lfMin > 0) return BoxDoubleDomain.this.bottom(dimension)

      val newlow = low.clone
      val newhigh = high.clone

      val coeffs = lf.homcoeffs
      val known = lf.known

      val infinities = (coeffs.indices) filter { i => lfArgmin(i).isInfinity && coeffs(i) != 0 }
      infinities.size match {
        case 0 =>
          for (i <- coeffs.indices) {
            if (coeffs(i) < 0) newlow(i) = low(i) max (lfArgmin(i) - lfMin / coeffs(i))
            if (coeffs(i) > 0) newhigh(i) = high(i) min (lfArgmin(i) - lfMin / coeffs(i))
          }
        case 1 => {
          val posinf = infinities.head
          if (coeffs(posinf) < 0)
            newlow(posinf) = low(posinf) max ((-dotprod_lo(coeffs, lfArgmin, posinf) - known) / coeffs(posinf))
          else
            newhigh(posinf) = high(posinf) min ((-dotprod_hi(coeffs, lfArgmin, posinf) - known) / coeffs(posinf))
        }
        case _ =>
      }
      BoxDoubleDomain.this(newlow, newhigh)
    }

    /**
     * @inheritdoc
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def linearDisequality(lf: LinearForm[Double]): Property = {
      val count = lf.homcoeffs.count(_ != 0)
      count match {
        case 0 =>
          if (lf.known == 0) bottom else this
        case 1 =>
          val dim = lf.homcoeffs.indexWhere(_ != 0)
          if (low(dim) == lf.known && high(dim) == lf.known)
            bottom
          else
            this
        case _ => this
      }
    }

    def addVariable: Property =
      if (isEmpty)
        BoxDoubleDomain.this.bottom(dimension + 1)
      else
        BoxDoubleDomain.this(low :+ Double.NegativeInfinity, high :+ Double.PositiveInfinity)

    /**
     * @inheritdoc
     * This is a complete operator for boxes.
     * @note @inheritdoc
     * @throws $ILLEGAL
     */
    def delVariable(n: Int): Property = {
      require(n < low.length && n >= 0)
      val newlow = new Array[Double](dimension - 1)
      val newhigh = new Array[Double](dimension - 1)
      Array.copy(low, 0, newlow, 0, n)
      Array.copy(high, 0, newhigh, 0, n)
      Array.copy(low, n + 1, newlow, n, dimension - n - 1)
      Array.copy(high, n + 1, newhigh, n, dimension - n - 1)
      new Property(newlow, newhigh, isEmpty)
    }

    /**
     * @inheritdoc
     * This is a complete operator for boxes.
     * @note @inheritdoc
     * @throws IllegalArgumentException if parameters are not correct (but we do not check injectivity of `rho`)
     */
    def mapVariables(rho: Seq[Int]) = {
      require(rho.length == dimension)
      val newdim = rho.count(_ >= 0)
      require(rho forall { i => i >= -1 && i < newdim })
      // we do not check injectivity
      val newlow = new Array[Double](newdim)
      val newhigh = new Array[Double](newdim)
      for ((newi, i) <- rho.zipWithIndex; if newi >= 0) {
        newlow(newi) = low(i)
        newhigh(newi) = high(i)
      }
      new Property(newlow, newhigh, isEmpty)
    }

    /**
     * @inheritdoc
     * @throws $ILLEGAL
     */
    def mkString(vars: Seq[String]): String = {
      require(vars.length >= dimension)
      if (isEmpty)
        "empty"
      else {
        val bounds = for (i <- 0 until dimension) yield {
          if (low(i) < high(i))
            low(i) + " <= " + vars(i) + " <= " + high(i)
          else vars(i) + " = " + high(i)
        }
        bounds.mkString("[ ", " , ", " ]")
      }
    }

    val dimension: Int = low.length

    def isBottom = isEmpty

    def isTop = !isEmpty && low.forall(_.isNegInfinity) && high.forall(_.isPosInfinity)

    def bottom = BoxDoubleDomain.this.bottom(low.length)

    def top = BoxDoubleDomain.this.top(low.length)

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
      case other: Property =>
        require(dimension == other.dimension)
        (isEmpty, other.isEmpty) match {
          case (true, true) => Some(0)
          case (false, true) => Some(1)
          case (true, false) => Some(-1)
          case (false, false) =>
            val lowpairs = (this.low, other.low).zipped
            val highpairs = (this.high, other.high).zipped
            if (lowpairs.forall(_ == _) && highpairs.forall(_ == _))
              Some(0)
            else if (lowpairs.forall(_ <= _) && highpairs.forall(_ >= _))
              Some(1)
            else if (lowpairs.forall(_ >= _) && highpairs.forall(_ <= _))
              Some(-1)
            else
              None
        }
      case _ => None
    }

    override def hashCode: Int = 41 * (41 + low.hashCode) + high.hashCode
  }

  /**
   * Returns a normalized box with given bounds.
   * @param low lower bounds.
   * @param high upper bounds.
   * @note `low` should have the same length as `high`.
   * @return the normalized box with the specified bounds.
   * @throws $ILLEGAL
   */
  def apply(low: Array[Double], high: Array[Double]): Property = {
    require(low.length == high.length)
    if ((low, high).zipped.exists(_ > _))
      bottom(low.length)
    else
      new Property(low, high, false)
  }

  /**
   * Returns a box consisting of the single point `poDouble`.
   */
  def apply(poDouble: Array[Double]): Property = apply(poDouble, poDouble)

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def top(n: Int): Property =
    new Property(Array.fill(n)(Double.NegativeInfinity), Array.fill(n)(Double.PositiveInfinity), false)

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def bottom(n: Int): Property =
    new Property(Array.fill(n)(Double.PositiveInfinity), Array.fill(n)(Double.NegativeInfinity), true)
}

object BoxDoubleDomain {
  /**
   * Returns an abstract domain for boxes which is correct w.r.t. real arithmetic or
   * double arithmetic, according to the parameter `overReals`.
   */
  def apply(overReals: Boolean = false) = if (overReals) this.overReals else this.overDoubles
  /**
   * The domain of boxes correct w.r.t. reals and with cached top and bottom.
   */
  private val overReals = new BoxDoubleDomain(true) with CachedTopBottom

  /**
   * The domain of boxes correct w.r.t. doubles and with cached top and bottom.
   */
  private val overDoubles = new BoxDoubleDomain(false) with CachedTopBottom
}
