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

package it.unich.sci.jandom.domains

import breeze.linalg._

/**
 * This is the abstract domain of parallelotopes as appears in the NSAD 2012 paper. It is written
 * using the Breeze Math library. It is not safe to use, since it does not implement rounding correctly.
 * Real domains should be implemented within $PPL or $APRON.
 * @author Gianluca Amato <g.amato@unich.it>
 *
 * @constructor Builds a `Parallelotope`.
 * @param isEmpty is true if the parallelotope is empty. In this case, the other parameters are not relevant.
 * @param A is the constraint matrix. It should be invertible.
 * @param low lower bounds.
 * @param high higher bounds.
 * @note `low` and `high` should have the same length. The  matrix `A` should be invertiible
 * of the same size of the two vectors.
 * @throws IllegalArgumentException if `low` and `high` are not of the same length, if `A` is not
 * square or if `A` has not the same size of `low`.
 */

class Parallelotope(
  val isEmpty: Boolean,
  private[domains] val low: DenseVector[Double],
  private[domains] val A: DenseMatrix[Double],
  private[domains] val high: DenseVector[Double])
  extends NumericalProperty[Parallelotope] {

  require((low.length == A.rows) && (low.length == A.cols) && (low.length == high.length))

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
      val (l1, u1) = Parallelotope.extremalsInBox(y1, low, high)
      val y2 = that.A.t \ v
      val (l2, u2) = Parallelotope.extremalsInBox(y2, that.low, that.high)
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
    val pvt = Parallelotope.pivoting(Qsorted map (_._1))

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
    val result = that.rotate(A)
    for (i <- 0 to dimension - 1) {
      result.low(i) = result.low(i) min low(i)
      result.low(i) = result.high(i) max high(i)
    }
    return result
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
    val result = that.rotate(A)
    for (i <- 0 to dimension - 1) {
      result.low(i) = result.low(i) max low(i)
      result.low(i) = result.high(i) min high(i)
    }
    return result
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @todo @inheritdoc
   * @throws $ILLEGAL
   */
  def linearAssignment(n: Int, tcoeff: Array[Double], known: Double): Parallelotope = {
    require(n <= dimension && tcoeff.length <= dimension)
    if (isEmpty) return this
    val coeff = tcoeff.padTo(dimension, 0.0).toArray
    if (coeff(n) != 0) {
      val increment = A(::, n) :* known / coeff(n)
      val newlow = low :+ increment
      val newhigh = high :+ increment
      val ei = SparseVector.zeros[Double](dimension)
      ei(n) = 1
      val newA = A :- (A(::, n) * (DenseVector(coeff) - ei).t) / coeff(n)
      new Parallelotope(false, newlow, newA, newhigh)
    } else {
      val newP = nonDeterministicAssignment(n)
      val Aprime = newP.A
      val j = ((0 to Aprime.rows - 1) find { Aprime(_, n) != 0 }).get
      for (s <- 0 to dimension - 1 if Aprime(s, n) != 0 && s != j)
        Aprime(s, ::) :-= Aprime(j, ::) * Aprime(s, n) / Aprime(j, n)
      val ei = DenseVector.zeros[Double](dimension)
      ei(n) = 1
      Aprime(j, ::) := ei :- DenseVector(coeff)
      newP.low(j) = known
      newP.high(j) = known
      return newP
    }
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @todo @inheritdoc
   * @throws ILLEGAL
   */
  def linearInequality(tcoeff: Array[Double], known: Double): Parallelotope = {
    require(tcoeff.length <= dimension)
    val coeff = tcoeff.padTo(dimension, 0.0).toArray
    if (isEmpty) return this
    val y = A.t \ DenseVector(coeff)
    val j = (0 to dimension - 1) find { i => y(i) != 0 && low(i).isInfinity && high(i).isInfinity }
    j match {
      case None => {
        val newlow = low.copy
        val newhigh = high.copy
        val (minc, maxc) = Parallelotope.extremalsInBox(y, newlow, newhigh)
        if (minc > -known) return Parallelotope.empty(dimension)
        for (i <- 0 to dimension - 1) {
          if (y(i) > 0)
            newhigh(i) = high(i) min ((-known - minc + y(i) * low(i)) / y(i))
          else if (y(i) < 0)
            newlow(i) = low(i) max ((-known - minc + y(i) * low(i)) / y(i))
        }
        return new Parallelotope(false, newlow, A, newhigh)
      }
      case Some(j) => {
        val newA = A.copy
        val newhigh = high.copy
        newA(j, ::) := DenseVector(coeff)
        newhigh(j) = -known
        return new Parallelotope(false, low, newA, newhigh)
      }
    }
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @note Not implemented yet
   * @throws $ILLEGAL
   */
  def linearDisequality(coeff: Array[Double], known: Double): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
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

  def addDimension: Parallelotope = {
    val e = DenseMatrix.zeros[Double](dimension + 1, 1)
    e(dimension, 0) = 1.0
    val newA = DenseMatrix.horzcat(DenseMatrix.vertcat(A, DenseMatrix.zeros[Double](1, dimension)), e)
    val newlow = DenseVector.vertcat(low, DenseVector(Double.NegativeInfinity))
    val newhigh = DenseVector.vertcat(high, DenseVector(Double.PositiveInfinity))
    new Parallelotope(false, newlow, newA, newhigh)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @note Not yet implemented.
   * @throws $ILLEGAL
   */
  def delDimension(n: Int): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
  }

  /**
   * @inheritdoc
   * @note Not yet implemented.
   */
  def mapDimensions(rho: Seq[Int]): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
  }

  def dimension = A.rows

  val isFull = low.forallValues(_.isNegInfinity) && high.forallValues(_.isPosInfinity)

  def empty = Parallelotope.empty(dimension)

  def full = Parallelotope.full(dimension)

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

  def <=[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) ⇒ PartiallyOrdered[B]): Boolean = {
    if (isEmpty) return (true)
    if (that.isEmpty) return (false)
    val ptemp = this.rotate(that.A)
    (0 to ptemp.low.length - 1) forall { i => ptemp.low(i) >= that.low(i) && ptemp.high(i) <= that.high(i) }
  }

  def >=[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) ⇒ PartiallyOrdered[B]): Boolean =
    that <= this

  def <[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) ⇒ PartiallyOrdered[B]): Boolean =
    (this <= that) && !(this >= that)

  def >[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) ⇒ PartiallyOrdered[B]): Boolean =
    (this >= that) && !(this <= that)

  override def equals(other: Any): Boolean = other match {
    case other: Parallelotope => (this <= other) && (other <= this)
    case _ => false
  }

  def mkString(vars: IndexedSeq[String]): Seq[String] = {

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
      Seq("empty")
    else
      for (i <- 0 until dimension) yield low(i) + " <= " + lfToString(A.t(::, i)) + " <= " + high(i)
  }
}

/**
 * The companion object for the Parallelotope abstract domain.
 */
object Parallelotope extends NumericalDomain {

  type Property = Parallelotope

  /**
   * Build a non-empty parallelotope. If the parallelotope is not empty, the
   * result is undetermined.
   * @param A is the constraint matrix. It should be invertible.
   * @param low lower bounds.
   * @param high higher bounds.
   * @note `low` and `high` should have the same length. The  matrix `A` should be invertiible
   * of the same size of the two vectors.
   * @throws IllegalArgumentException if `low` and `high` are not of the same length, if `A` is not
   * square or if `A` has not the same size of `low`.
   */

  def apply(low: DenseVector[Double], A: DenseMatrix[Double], high: DenseVector[Double]) =
    new Parallelotope(false, low, A, high)

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */

  def full(n: Int) = {
    val low = DenseVector.fill(n)(Double.NegativeInfinity)
    val high = DenseVector.fill(n)(Double.PositiveInfinity)
    val A = DenseMatrix.eye[Double](n)
    new Parallelotope(false, low, A, high)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def empty(n: Int) = {
    val low = DenseVector.fill(n)(1.0)
    val high = DenseVector.fill(n)(0.0)
    val A = DenseMatrix.eye[Double](n)
    new Parallelotope(true, low, A, high)
  }

  /**
   * Given the box specified by `low` and `high` and the linear form `lf`, determines the pair
   * of the least and greatest value of the linear form in the box.
   * @param lf  a linear form.
   * @param low lower bound of the box.
   * @param high higher bound of the box.
   * @note `low`, `high` and `lf` should be of the same length.
   * @return the least and greatest value of `lf` in the box determine by `low` and `high`.
   */
  private def extremalsInBox(lf: DenseVector[Double], low: DenseVector[Double], high: DenseVector[Double]): (Double, Double) = {
    var minc = 0.0
    var maxc = 0.0
    for (i <- 0 to lf.length - 1)
      if (lf(i) > 0) {
        minc += lf(i) * low(i)
        maxc += lf(i) * high(i)
      } else if (lf(i) < 0) {
        minc += lf(i) * high(i)
        maxc += lf(i) * low(i)
      }
    return (minc, maxc)
  }

  /**
   * Given a sequence of vectors of the same length `n`, returns a sequence of `n` indexes
   * of vectors which are linearly independent. It is based on Gaussian elimination.
   * @param m a sequence of vectors, all of the same length.
   * @return a sequence of positions in m.
   */
  private def pivoting(m: IndexedSeq[DenseVector[Double]]): Seq[Int] = {
    val dimension = m(0).length
    var indexes = Seq[Int]()
    var pivots = Seq[(DenseVector[Double], Int)]()
    var i = 0
    while (indexes.length < dimension) {
      val row = m(i).copy
      for (p <- pivots) row -= p._1 * row(p._2)
      val col = (0 to row.length - 1) find (row(_) != 0)
      col match {
        case Some(col) =>
          row /= row(col)
          pivots :+= (row, col)
          indexes :+= i
        case None =>
      }
      i += 1
    }
    indexes
  }
}
