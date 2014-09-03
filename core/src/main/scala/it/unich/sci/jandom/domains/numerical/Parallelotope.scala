/**
 * Copyright 2013 Gianluca Amato, Francesca Scozzari
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.sci.jandom.domains.numerical

import breeze.linalg._
import scala.Array.canBuildFrom
import scala.util.Try

/**
 * This is an element of the parallelotope domain.
 *
 * @constructor Builds a parallelotope
 * @param isEmpty is true if the parallelotope is empty. In this case, the other parameters are not relevant.
 * @param A is the constraint matrix. It should be invertible.
 * @param low lower bounds.
 * @param high higher bounds.
 * @note `low` and `high` should have the same length. The  matrix `A` should be invertible
 * of the same size of the two vectors.
 * @throws IllegalArgumentException if `low` and `high` are not of the same length, if `A` is not
 * square or if `A` has not the same size of `low`.
 */

final class Parallelotope (
  val isEmpty: Boolean,
  private[domains] val low: DenseVector[Double],
  private[domains] val A: DenseMatrix[Double],
  private[domains] val high: DenseVector[Double])
  extends NumericalProperty[Parallelotope] {

  require(low.length == A.rows)
  require(low.length == A.cols)
  require(Try(A \ DenseMatrix.eye[Double](dimension)).isSuccess,s"The shape matrix ${A} is not invertible")
  require(normalized)

  type Domain = ParallelotopeDomain

  def domain = ParallelotopeDomain()

  private def normalized: Boolean = {
    low.length == high.length && (
      (0 until low.length - 1) forall { i =>
        !low(i).isPosInfinity &&
          !high(i).isNegInfinity &&
          (low(i) <= high(i) || isEmpty)
      })
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def widening(that: Parallelotope): Parallelotope = {
    require(dimension == that.dimension)
    if (isEmpty) return that
    val thatRotated = that.rotate(A)
    val newlow = low.copy
    val newhigh = high.copy
    for (i <- 0 to dimension - 1) {
      if (thatRotated.low(i) < low(i)) newlow(i) = Double.NegativeInfinity
      if (thatRotated.high(i) > high(i)) newhigh(i) = Double.PositiveInfinity
    }
    new Parallelotope(false, newlow, A, newhigh)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def narrowing(that: Parallelotope): Parallelotope = {
    require(dimension == that.dimension)
    if (isEmpty) this
    val thatRotated = that.rotate(A)
    val newlow = low.copy
    val newhigh = high.copy
    for (i <- 0 to dimension - 1) {
      if (low(i).isInfinity) newlow(i) = thatRotated.low(i) else newlow(i) = newlow(i) min thatRotated.low(i)
      if (high(i).isInfinity) newhigh(i) = thatRotated.high(i) else newhigh(i) = newhigh(i) max thatRotated.high(i)
    }
    new Parallelotope(false, newlow, A, newhigh)
  }

  /**
   * @inheritdoc
   * It is equivalent to `intersectionWeak`.
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def intersection(that: Parallelotope): Parallelotope = intersectionWeak(that)

  /**
   * @inheritdoc
   * The union of two parallelotopes is not a parallelotope. Moreover, there is no least
   * parallelotopes containing the union of two parallelotopes. Hence, this methods uses
   * heuristics to find a good result.
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def union(that: Parallelotope): Parallelotope = {
  
    /**
     * A PrioritizedConstraint is a tuple `(a, m, M, p)` where `a` is a vector, `m` and `M` are
     * reals and `p` is an integer, whose intended meaning is the constraint `m <= ax <= M`
     * with a given priority `p`.
     */
    type PrioritizedConstraint = (DenseVector[Double], Double, Double, Int)

    /**
     * Given a linear form `v`, compute a prioritized constraint `(v,m,M,p)`. The parameter `ownedBy`
     * tells whether the line form under consideration is one of the "native" forms of this (1) or
     * that (2). This is used to refine priorities.
     */
    def priority(v: DenseVector[Double], ownedBy: Int = 0): PrioritizedConstraint = {

      val y1 = A.t \ v
      val (l1, u1) = domain.extremalsInBox(y1, low, high)
      val y2 = that.A.t \ v
      val (l2, u2) = domain.extremalsInBox(y2, that.low, that.high)
      val p =
        if (l1 == l2 && l2 == u1 && u1 == u2)
          0
        else if (!l1.isInfinity && !l2.isInfinity && !u1.isInfinity && !u2.isInfinity) {
          if (l1 == l2 && u1 == u2)
            1
          else if (l1 >= l2 && u1 <= u2)
            if (ownedBy == 2) 2 else 3
          else if (l2 >= l1 && u2 <= u1)
            if (ownedBy == 1) 2 else 3
          else if (l2 <= u1 && l2 >= l1 && u2 >= u1)
            3
          else if (l2 <= l1 && u2 >= l1 && u2 <= u1)
            3
          else 4
        } else if (l1 == l2 && !l1.isInfinity && !l2.isInfinity)
          5
        else if (u1 == u2 && !u1.isInfinity && !u2.isInfinity)
          5
        else if (!l1.isInfinity && !l2.isInfinity)
          6
        else if (!u1.isInfinity && !u2.isInfinity)
          6
        else 100
      return (v, l1 min l2, u1 max u2, p)
    }

    /**
     * Determines whether `v1` and `v2` are linearly dependent.
     * @return `None` if `v1` and `v2` are not linearly dependent, otherwise it is
     * `Some(k)` such that `v1 = k * v2`.
     */
    def linearDep(v1: DenseVector[Double], v2: DenseVector[Double]): Option[Double] = {
      var i: Int = 0
      while (i < dimension && (v1(i) == 0 || v2(i) == 0)) i += 1
      if (i == dimension)
        Some(1)
      else if (v1 / v1(i) == v2 / v2(i))
        return Some(v1(i) / v2(i))
      else
        None
    }

    /**
     * The inversion join procedure.
     * @param vi first vector
     * @param vj second vector
     * @param min1i least value of v1 in this
     * @param min2i least value of v1 in that
     * @param min1j least value of v2 in that
     * @param min2j least value of v2 in that
     * @return None if `v1` and `v2` do not form an inversion, otherwise it is `Some(v)` where `v` is the
     * new linear form computed by the inversion procedure.
     */
    def newConstraint(vi: DenseVector[Double], vj: DenseVector[Double], min1i: Double, min2i: Double, min1j: Double, min2j: Double): Option[DenseVector[Double]] = {
      if (min1i.isInfinity || min2i.isInfinity || min1j.isInfinity || min2j.isInfinity) return None
      if (linearDep(vi, vj).isEmpty) return None
      val (deltai, deltaj) = if (min2j - min1j >= 0) (min1i - min2i, min2j - min1j) else (min2i - min1i, min1j - min2j)
      if (deltai * deltaj > 0)
        Some(-vi * deltaj - vj * deltai)
      else
        None
    }

    require(dimension == that.dimension)
    
    if (isEmpty) return that
    if (that.isEmpty) return this
    val thisRotated = this.rotate(that.A)
    val thatRotated = that.rotate(this.A)
    val Q = scala.collection.mutable.ArrayBuffer[PrioritizedConstraint]()

    val bulk = DenseMatrix.vertcat(this.A, that.A)
    val min1 = DenseVector.vertcat(this.low, thisRotated.low)
    val min2 = DenseVector.vertcat(thatRotated.low, that.low)
    val max1 = DenseVector.vertcat(this.high, thisRotated.high)
    val max2 = DenseVector.vertcat(thatRotated.high, that.high)
    
    for (i <- 0 to dimension - 1) Q += priority(this.A.t(::, i), 1)
    for (i <- 0 to dimension - 1) Q += priority(that.A.t(::, i), 2)
    for (i <- 0 to dimension - 1; j <- i + 1 to dimension - 1) {
      val v1 = bulk.t(::, i)
      val v2 = bulk.t(::, j)
      val nc1 = newConstraint(v1, v2, min1(i), min2(i), min1(j), min2(j))
      if (nc1.isDefined) Q += priority(nc1.get)
      val nc2 = newConstraint(v1, -v2, min1(i), min2(i), -max1(j), -max2(j))
      if (nc2.isDefined) Q += priority(nc2.get)
      val nc3 = newConstraint(-v1, -v2, -max1(i), -max2(i), -max1(j), -max2(j))
      if (nc3.isDefined) Q += priority(nc3.get)
      val nc4 = newConstraint(-v1, v2, -max1(i), -max2(i), min1(j), min2(j))
      if (nc4.isDefined) Q += priority(nc4.get)
    }
    val Qsorted = Q.sortBy[Int](_._4)
    val pvt = domain.pivoting(Qsorted map (_._1))

    val newA = DenseMatrix(pvt map (Qsorted(_)._1.toArray): _*)
    val newlow = DenseVector(pvt map (Qsorted(_)._2): _*)
    val newhigh = DenseVector(pvt map (Qsorted(_)._3): _*)

    new Parallelotope(false, newlow, newA, newhigh)
  }

  /**
   * This is a variant of `union` using weak join. The shape of the resulting
   * parallelotope is the same shap of `this`.
   * @param that the abstract object to be joined with `this`.
   * @note $NOTEDIMENSION
   * @return the weak union of the two abstract objects.
   */
  def unionWeak(that: Parallelotope): Parallelotope = {
    require(dimension == that.dimension)
    if (isEmpty) return that
    if (that.isEmpty) return this
    val result = that.rotate(A)
    for (i <- 0 to dimension - 1) {
      result.low(i) = result.low(i) min low(i)
      result.high(i) = result.high(i) max high(i)
    }
    new Parallelotope(false, result.low, result.A, result.high) //this is to normalize
  }

  /**
   * This is the weak intersection of two abstract objects. The shape of the resulting
   * parallelotope is the same shap of `this`.
   * @param that the abstract object to be intersected with `this`.
   * @note $NOTEDIMENSION
   * @return the intersection of the two abstract objects.
   */
  def intersectionWeak(that: Parallelotope): Parallelotope = {
    require(dimension == that.dimension)
    if (isEmpty) return this
    if (that.isEmpty) return that
    val result = that.rotate(A)
    for (i <- 0 to dimension - 1) {
      result.low(i) = result.low(i) max low(i)
      result.high(i) = result.high(i) min high(i)
    }
    if ((0 until result.low.length) exists { i => (result.low(i) > result.high(i)) })
      bottom
    else new Parallelotope(false, result.low, result.A, result.high) //this is to normalize
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @todo @inheritdoc
   * @throws $ILLEGAL
   */
  def linearAssignment(n: Int, lf: LinearForm[Double]): Parallelotope = {
    require(n <= dimension && lf.dimension <= dimension)
    val tcoeff = lf.homcoeffs
    val known = lf.known
    if (isEmpty) return this
    val coeff = tcoeff.padTo(dimension, 0.0).toArray
    if (coeff(n) != 0) {
      val increment = A(::, n) :* known / coeff(n)
      val newlow = low :+ increment
      val newhigh = high :+ increment
      // in the past (4.0) we could use SparseVector instead of DenseVector, but this does
      // not work anymore
      val ei = DenseVector.zeros[Double](dimension)
      ei(n) = 1
      val newA = A :- (A(::, n) * (DenseVector(coeff) - ei).t) / coeff(n)
      new Parallelotope(false, newlow, newA, newhigh)
    } else {
      val newP = nonDeterministicAssignment(n)
      val Aprime = newP.A.copy
      val j = ((0 to Aprime.rows - 1) find { Aprime(_, n) != 0 }).get
      for (s <- 0 to dimension - 1 if Aprime(s, n) != 0 && s != j)
        Aprime(s, ::) :-= Aprime(j, ::) * Aprime(s, n) / Aprime(j, n)
      val ei = DenseVector.zeros[Double](dimension)
      ei(n) = 1
      Aprime(j, ::) := (ei :- DenseVector(coeff)).t
      val newlow = newP.low.copy
      val newhigh = newP.high.copy
      newlow(j) = known
      newhigh(j) = known
      return new Parallelotope(false, newlow, Aprime, newhigh)
    }
  }

  private def dotprod(x: DenseVector[Double], y: DenseVector[Double], remove: Int = -1): Double = {
      var sum: Double = 0
      for (i <- 0 until x.length if i != remove if x(i) != 0) sum = sum + x(i)*y(i)
      sum
    }
  
  /**
   * @inheritdoc
   * @note @inheritdoc
   * @todo @inheritdoc
   * @throws ILLEGAL
   */
  def linearInequality(lf: LinearForm[Double]): Parallelotope = {
  
    require(lf.dimension <= dimension)
    
    if (isEmpty) return this

    val tcoeff = lf.homcoeffs
    val known = lf.known
    val coeff = tcoeff.padTo(dimension, 0.0).toArray
    
    val y = A.t \ DenseVector(coeff)
    val j = (0 until dimension) find { i => y(i) != 0 && low(i).isInfinity && high(i).isInfinity }
    j match {
      case None => {
        val newlow = low.copy
        val newhigh = high.copy
        val (minc, maxc) = domain.extremalsInBox(y, newlow, newhigh)
        if (minc > -known) return domain.bottom(dimension)
       
       
        val lfArgmin = (y) mapPairs { case (i, c) => if (c > 0) low(i) else high(i) }
        
        val infinities = (0 until dimension) filter { i => lfArgmin(i).isInfinity && y(i) != 0 }
        infinities.size match {
        	case 0 =>
        		for (i <- 0 until dimension) {
        			if (y(i) > 0) newhigh(i) = high(i) min (lfArgmin(i) + (-known - minc) / y(i))
        			else if (y(i) < 0) newlow(i) = low(i) max (lfArgmin(i) + (-known - minc) / y(i))
        		}
       	    case 1 => {	
        	val posinf = infinities.head
        	if (y(posinf) < 0)
        		newlow(posinf) = low(posinf) max ((-dotprod(y, lfArgmin, posinf) - known) / y(posinf))
            else
            	newhigh(posinf) = high(posinf) min ((-dotprod(y, lfArgmin, posinf) - known) / y(posinf))
        }
        case _ =>
      }
        return new Parallelotope(false, newlow, A, newhigh)
      }
      case Some(j) => {
        val newA = A.copy
        val newhigh = high.copy
        newA(j, ::) := DenseVector(coeff).t
        newhigh(j) = -known
        return new Parallelotope(false, low, newA, newhigh)
      }
    }
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def linearDisequality(lf: LinearForm[Double]): Parallelotope = {
    val tcoeff = lf.homcoeffs
    val known = lf.known
    if (tcoeff.forall(_ == 0))
      if (known == 0) bottom else this
    else {
      val row = (0 until dimension).find(A(_, ::).t == DenseVector(tcoeff: _*))
      row match {
        case None => this
        case Some(row) =>
          if (low(row) == known && high(row) == known) bottom else this
      }
    }
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def nonDeterministicAssignment(n: Int): Parallelotope = {
    require(n <= dimension)
    if (isEmpty) return this;
    val j = (0 to dimension - 1).filter { i => A(i, n) != 0 && (!low(i).isNegInfinity || !high(i).isPosInfinity) }
    if (j.isEmpty) return this
    val newA = A.copy
    val newlow = low.copy
    val newhigh = high.copy
    val j0 = j.filter { i => low(i) == high(i) }
    val j1 = j.filter { i => !low(i).isInfinity && !high(i).isInfinity }
    val r = if (!j0.isEmpty) j0(0) else if (!j1.isEmpty) j1(0) else j(0)
    val rowr = A(r, ::)
    for (i <- j if i != r) {
      val value1 = A(r, n)
      val value2 = A(i, n)
      val rowi = A(i, ::)
      newA(i, ::) := rowr * value2 - rowi * value1
      val (minr, maxr) = if (A(i, n) < 0) (high(r), low(r)) else (low(r), high(r))
      val (mini, maxi) = if (-A(r, n) < 0) (high(i), low(i)) else (low(i), high(i))
      newlow(i) = minr * value2 - mini * value1
      newhigh(i) = maxr * value2 - maxi * value1
    }
    newlow(r) = Double.NegativeInfinity
    newhigh(r) = Double.PositiveInfinity
    new Parallelotope(false, newlow, newA, newhigh)
  }

  def addVariable(): Parallelotope = {
    if (isEmpty)
      domain.bottom(A.rows + 1)
    else {
      val e = DenseMatrix.zeros[Double](dimension + 1, 1)
      e(dimension, 0) = 1.0
      val newA = DenseMatrix.horzcat(DenseMatrix.vertcat(A, DenseMatrix.zeros[Double](1, dimension)), e)
      val newlow = DenseVector.vertcat(low, DenseVector(Double.NegativeInfinity))
      val newhigh = DenseVector.vertcat(high, DenseVector(Double.PositiveInfinity))
      new Parallelotope(false, newlow, newA, newhigh)
    }
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def delVariable(n: Int): Parallelotope = {
    if (isEmpty)
      domain.bottom(A.rows - 1)
    else {
      val slice = (0 until n) ++ (n + 1 until dimension)
      val newA = A(slice, slice).toDenseMatrix
      val newlow = low(slice).toDenseVector
      val newhigh = high(slice).toDenseVector
      new Parallelotope(false, newlow, newA, newhigh)
    }
  }

  /**
   * @inheritdoc
   */
  def mapVariables(rho: Seq[Int]): Parallelotope = {
    if (isEmpty)
      this
    else {
      val slice = for (i <- 0 until dimension; j = rho.indexOf(i); if j != -1) yield j
      val newA = A(slice, slice).toDenseMatrix
      val newlow = low(slice).toDenseVector
      val newhigh = high(slice).toDenseVector
      new Parallelotope(false, newlow, newA, newhigh)
    }
  }

  /**
   * Compute the minimum and maximum value of a linear form in a parallelotope.
   * @todo should be generalized to linear forms over arbitrary types.
   * @return a tuple with two components: the first component is the least value, the second component is the greatest value
   * of the linear form over the box.
   */
  def linearEvaluation(lf: LinearForm[Double]): (Double, Double) = {
    
    val tcoeff = lf.homcoeffs
    val coeff = tcoeff.padTo(dimension, 0.0).toArray
    val vec = DenseVector(coeff)
    val newvec = A.t \ vec
    val newlf = lf.known +: newvec.valuesIterator.toSeq
    val (min,max) = domain.extremalsInBox(newvec, low, high)
    (min + lf.known, max + lf.known)
  }

  def minimize(lf: LinearForm[Double]) = linearEvaluation(lf)._1

  def maximize(lf: LinearForm[Double]) = linearEvaluation(lf)._2

  def frequency(lf: LinearForm[Double]) = {
    val (min, max) = linearEvaluation(lf)
    if (min == max) Some(min) else None
  }

  def dimension = A.rows

  def isTop = low.forallValues(_.isNegInfinity) && high.forallValues(_.isPosInfinity)

  def isBottom = isEmpty

  def bottom = domain.bottom(dimension)

  def top = domain.top(dimension)

  def tryCompareTo[B >: Parallelotope](that: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = that match {
    case that: Parallelotope =>
      val lte = this <= that
      val gte = that <= this
      if (lte && gte)
        Some(0)
      else if (lte)
        Some(-1)
      else if (gte)
        Some(1)
      else
        None
    case _ => None
  }

  /**
   * It computes the smallest parallelotope which containts `this` and is definable
   * over a new shape matrix `Aprime`.
   * @param Aprime the new shape matrix.
   * @note `Aprime` should be an invertible matrix of the same dimension as `this`.
   * @throws IllegalArgumentException if `Aprime` is not square or has not the correct dimension.
   */
  def rotate(Aprime: DenseMatrix[Double]): Parallelotope = {
    require(dimension == Aprime.rows && dimension == Aprime.cols)
    if (isEmpty) return this;
    val B = Aprime * (A \ DenseMatrix.eye[Double](dimension))
    val newlow = DenseVector.zeros[Double](dimension)
    val newhigh = DenseVector.zeros[Double](dimension)
    B.foreachPair {
      case ((i, j), v) =>
        if (v > 0) {
          newlow(i) += v * low(j)
          newhigh(i) += v * high(j)
        } else if (v < 0) {
          newhigh(i) += v * low(j)
          newlow(i) += v * high(j)
        }
    }
    return new Parallelotope(false, newlow, Aprime, newhigh)
  }

  def <=[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean = {
    if (isEmpty) return (true)
    if (that.isEmpty) return (false)
    if (that.isTop) return (true)
    val ptemp = this.rotate(that.A)
    (0 to ptemp.low.length - 1) forall { i => ptemp.low(i) >= that.low(i) && ptemp.high(i) <= that.high(i) }
  }

  def >=[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean =
    that <= this

  def <[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean =
    (this <= that) && !(this >= that)

  def >[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) => PartiallyOrdered[B]): Boolean =
    (this >= that) && !(this <= that)

  def mkString(vars: Seq[String]): String = {

    /**
     * Returns a string representation of the linear form `lf`.
     */
    def lfToString(lf: DenseVector[Double]): String = {
      var first = true
      var s = ""

      for (index <- 0 until dimension) {
        val coeff = lf(index)
        val term = coeff match {
          case 0 => ""
          case 1 => vars(index)
          case -1 => "-" + vars(index)
          case c => c.toString + "*" + vars(index)
        }
        if (coeff != 0) {
          if (first || coeff < 0) {
            s += term
            first = false
          } else if (coeff != 0)
            s += "+" + term
        }
      }
      if (s.isEmpty) "0" else s
    }

    if (isEmpty)
      "empty"
    else {
      val eqns = for (i <- 0 until dimension) yield {
        if (low(i) < high(i))
          low(i) + " <= " + lfToString(A.t(::, i)) + " <= " + high(i)
        else lfToString(A.t(::, i)) + " = " + high(i)
      }
      eqns.mkString("[ ", " , ", " ]")
    }
  }
}
