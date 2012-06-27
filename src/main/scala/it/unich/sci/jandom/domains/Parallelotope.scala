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
 * (c) 2012 Gianluca Amato
 */

package it.unich.sci.jandom
package domains

import scalala.library.LinearAlgebra
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.::
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.DenseVector

/**
 * This is the abstract domain of parallelotopes.
 * @author Gianluca Amato <g.amato@unich.it>
 */
class Parallelotope(
  val isEmpty: Boolean,
  private val low: DenseVector[Double],
  private val A: DenseMatrix[Double],
  private val high: DenseVector[Double])
  extends NumericalProperty[Parallelotope] {

  require((low.length == A.numRows) && (low.length == high.length) && A.isSquare)

  def widening(that: Parallelotope): Parallelotope = {
    require(dimension == that.dimension)
    if (isEmpty) that
    val thatRotated = that.rotate(A)
    val newlow = low.toDense
    val newhigh = high.toDense
    for (i <- 0 to dimension-1) {
      if (thatRotated.low(i)<low(i)) newlow(i)=Double.NegativeInfinity
      if (thatRotated.high(i)>high(i)) newhigh(i)=Double.PositiveInfinity
    }   
    new Parallelotope(false, newlow, A, newhigh)
  }
  
  def narrowing(that: Parallelotope): Parallelotope = {
    require(dimension == that.dimension)
    if (isEmpty) this
    val thatRotated = that.rotate(A)
    val newlow = low.toDense
    val newhigh = high.toDense
    for (i <- 0 to dimension-1) {
      if (low(i).isInfinity) newlow(i)=thatRotated.low(i) else newlow(i)=newlow(i) min thatRotated.low(i)
      if (high(i).isInfinity) newhigh(i)=thatRotated.high(i) else newhigh(i)=newhigh(i) max thatRotated.high(i)
    }   
    new Parallelotope(false, newlow, A, newhigh)
  }

  def intersection(that: Parallelotope): Parallelotope = intersectionWeak(that)

  def union(that: Parallelotope): Parallelotope = {

    type PrioritizedConstraint = (DenseVector[Double], Double, Double, Int)

    def priority(v: DenseVector[Double]): PrioritizedConstraint = {
      val y1 = A.t.toDense \ v.asCol.toDense
      val (l1, u1) = Parallelotope.extremalsInBox(y1, low, high)
      val y2 = that.A.t.toDense \ v.asCol.toDense
      val (l2, u2) = Parallelotope.extremalsInBox(y2, that.low, that.high)
      val p =
        if (l1 == l2 && l2 == u1 && u1 == u2)
          0
        else if (!l1.isInfinity && !l2.isInfinity && !u1.isInfinity && !u2.isInfinity) {
          if (l1 == l2 && u1 == u2)
            1
          else if (l2 <= u1 && l2 >= l1 && u2 >= u1)
            2
          else if (l2 <= l1 && u2 >= l1 && u2 <= u1)
            2
          else 3
        } else if (l1 == l2 && !l1.isInfinity && !l2.isInfinity)
          4
        else if (u1 == u2 && !u1.isInfinity && !u2.isInfinity)
          4
        else if (!l1.isInfinity && !l2.isInfinity)
          5
        else if (!u1.isInfinity && !u2.isInfinity)
          5
        else 7
      return (v, l1 min l2, u1 max u2, p)
    }

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
    val thisRotated = this.rotate(that.A)
    val thatRotated = that.rotate(this.A)
    val Q = scala.collection.mutable.ArrayBuffer[PrioritizedConstraint]()

    val bulk = DenseMatrix.vertcat(this.A, that.A)
    val min1 = DenseVector.vertcat(this.low.asCol, thisRotated.low.asCol)
    val min2 = DenseVector.vertcat(thatRotated.low.asCol, that.low.asCol)
    val max1 = DenseVector.vertcat(this.high.asCol, thisRotated.high.asCol)
    val max2 = DenseVector.vertcat(thatRotated.high.asCol, that.high.asCol)
    for (i <- 0 to dimension - 1) Q += priority(this.A(i, ::))
    for (i <- 0 to dimension - 1) Q += priority(that.A(i, ::))
    for (i <- 0 to dimension - 1; j <- i + 1 to dimension - 1) {
      val v1 = bulk(i, ::)
      val v2 = bulk(j, ::)
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

    val newA = DenseMatrix(Qsorted map (_._1.toArray): _*)
    val newlow = DenseVector(Qsorted map (_._2): _*)
    val newhigh = DenseVector(Qsorted map (_._3): _*) 
    val pvt = Parallelotope.pivoting(Qsorted map (_._1))
    return new Parallelotope(false, newlow(pvt).toDense, newA(pvt,::).toDense, newhigh(pvt).toDense)
  }

  def unionWeak(that: Parallelotope): Parallelotope = {
    require(dimension == that.dimension)
    val result = that.rotate(A)
    for (i <- 0 to dimension - 1) {
      result.low(i) = result.low(i) min low(i)
      result.low(i) = result.high(i) max high(i)
    }
    return result
  }
  
  def intersectionWeak(that: Parallelotope): Parallelotope = {
    require(dimension == that.dimension)
    val result = that.rotate(A)
    for (i <- 0 to dimension - 1) {
      result.low(i) = result.low(i) max low(i)
      result.low(i) = result.high(i) min high(i)
    }
    return result
  }
  
  def linearAssignment(n: Int, tcoeff: Array[Double], known: Double): Parallelotope = {
    require(n <= dimension && tcoeff.length <= dimension)
    if (isEmpty) return this    
    val coeff = tcoeff.padTo(dimension,0.0).toArray
    if (coeff(n) != 0) {
      val increment = A(::, n) :* known / coeff(n)
      val newlow = low :+ increment
      val newhigh = high :+ increment
      val ei = SparseVector.zeros[Double](dimension)
      ei(n) = 1
      val newA = A :- (A(::, n) * (DenseVector(coeff) - ei).asRow) / coeff(n)
      new Parallelotope(false, newlow, newA, newhigh)
    } else {
      val newP = nondeterministicAssignment(n)
      val Aprime = newP.A
      val j = (Aprime(::, n) find { _ != 0 }).get
      for (s <- 0 to dimension - 1 if Aprime(s, n) != 0 && s != j)
        Aprime(s, ::) :-= Aprime(j, ::) * Aprime(s, n) / Aprime(j, n)
      val ei = SparseVector.zeros[Double](dimension)
      ei(n) = 1
      Aprime(j, ::) := ei :- DenseVector(coeff)
      newP.low(j) = known
      newP.high(j) = known
      return newP
    }
  }

  def linearInequality(tcoeff: Array[Double], known: Double): Parallelotope = {
    require(tcoeff.length <= dimension)
    val coeff = tcoeff.padTo(dimension,0.0).toArray
    if (isEmpty) return this
    val y = A.t.toDense \ DenseVector(coeff)
    val j = (0 to dimension - 1) find { i => y(i) != 0 && low(i).isInfinity && high(i).isInfinity }
    j match {
      case None => {
        val newlow = low.toDense
        val newhigh = high.toDense
        val (minc, maxc) = Parallelotope.extremalsInBox(y, newlow, newhigh)
        if (minc > known) return Parallelotope.empty(dimension)
        for (i <- 0 to dimension - 1) {
          if (y(i) > 0)
            newhigh(i) = high(i) min ((known - minc + y(i) * low(i)) / y(i))
          else if (y(i) < 0)
            newlow(i) = low(i) max ((known - minc + y(i) * low(i)) / y(i))
        }
        return new Parallelotope(false, newlow, A, newhigh)
      }
      case Some(j) => {
        val newA = A.copy
        val newhigh = high.toDense
        newA(j, ::) := DenseVector(coeff)
        newhigh(j) = known
        return new Parallelotope(false, low, newA, newhigh)
      }
    }
  }

  def linearDisequality(coeff: Array[Double], known: Double): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
  }

  /**
   * It computes the parallelotope which results after a non-deterministic assignment
   * to a variable.
   * @param n the variable we are applying a non-deterministic assignment.
   */
  def nondeterministicAssignment(n: Int): Parallelotope = {
    require(n <= dimension)
    if (isEmpty) return this;
    val j = (0 to dimension - 1).filter { i => A(i, n) != 0 && (!low(i).isNegInfinity || !high(i).isPosInfinity) }
    if (j.isEmpty) return this
    val newA = A.copy
    val newlow = low.toDense
    val newhigh = high.toDense
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

  val dimension = A.numCols

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
   * It computes the smallest parallelotope which containts this and is definable
   * over a new shape matrix.
   * @param Aprime the new shape matrix.
   */
  def rotate(Aprime: DenseMatrix[Double]): Parallelotope = {
    require(dimension == Aprime.numRows && dimension == Aprime.numCols)
    if (isEmpty) return this;
    val B = Aprime * (A \ DenseMatrix.eye[Double](dimension))
    val newlow = DenseVector.zeros[Double](dimension)
    val newhigh = DenseVector.zeros[Double](dimension)
    B.foreachNonZeroTriple {
      case (i, j, v) =>
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
    (ptemp.low :>= that.low).forallValues(identity[Boolean]) &&
      (ptemp.high :<= that.high).forallValues(identity[Boolean])
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

  override def toString = if (isEmpty) "<empty>" else "low:\n" + low + "\nhigh:\n" + high + "\nA:\n" + A
}

object Parallelotope extends NumericalDomain[Parallelotope] {

  def apply(low: DenseVector[Double], A: DenseMatrix[Double], high: DenseVector[Double]) =
    new Parallelotope(false, low, A, high)

  def full(n: Int) = {
    val low = DenseVector.fill(n)(Double.NegativeInfinity)
    val high = DenseVector.fill(n)(Double.PositiveInfinity)
    val A = DenseMatrix.eye[Double](n)
    new Parallelotope(false, low, A, high)
  }

  def empty(n: Int) = {
    val low = DenseVector.fill(n)(1.0)
    val high = DenseVector.fill(n)(0.0)
    val A = DenseMatrix.eye[Double](n)
    new Parallelotope(true, low, A, high)
  }
  
  
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
  
  def pivoting(m: IndexedSeq[DenseVector[Double]]): Seq[Int]=  {
    val dimension = m(0).length
    var indexes = Seq[Int]()
    var pivots = Seq[(DenseVector[Double],Int)]()
    var i = 0
    while (indexes.length <  dimension) {
      val row = m(i).toDense
      for (p <- pivots) row -= p._1 * row(p._2)
      val col = row find (_ != 0)
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
