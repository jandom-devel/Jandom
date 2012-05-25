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
import scalala.tensor.dense.DenseVectorCol
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.::
import scalala.tensor.sparse.SparseVector

/**
 * This is the abstract domain of parallelotopes.
 * @author Gianluca Amato <g.amato@unich.it>
 */
class Parallelotope(
    private val low: DenseVectorCol[Double],
    private val A: DenseMatrix[Double],
    private val high: DenseVectorCol[Double]) 
  extends NumericalProperty[Parallelotope] {

  require((low.length == A.numRows) && (low.length == high.length) && A.isSquare)

  def widening(that: Parallelotope): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
  }

  def narrowing(that: Parallelotope): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
  }

  def union(that: Parallelotope): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
  }

  def intersection(that: Parallelotope): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
  }

  def linearAssignment(n: Int, coeff: Array[Double], known: Double): Parallelotope = {
    if (coeff(n)!=0) {
      val increment = A(::,n) :* known/coeff(n) 
      val newlow = low :+ increment
      val newhigh = high :+ increment
      val ei = SparseVector.zeros[Double](dimension)
      ei(n) = 1
      val newA = A :- (A(::,n) * (DenseVectorCol(coeff) - ei).asRow)     
      new Parallelotope(newlow, newA, newhigh)
    } else {
      val indexes = (0 to dimension - 1).filter { i => A(i, n) != 0 && (!low(i).isNegInfinity || !high(i).isPosInfinity) }
      val newA = nondeterministicAssignment(n)
      throw new IllegalAccessException("Unimplemented feature")
    }
  }

  def linearInequality(coeff: Array[Double], known: Double): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
  }

  def linearDisequality(coeff: Array[Double], known: Double): Parallelotope = {
    throw new IllegalAccessException("Unimplemented feature")
  }

  /**
   * It computes the parallelotope which results after a non-deterministic assignment
   * to a variable, and records a line 
   * @param n the variable we are applying a non-deterministic assignment.
   */
  def nondeterministicAssignment(n: Int): Parallelotope = {
    val indexes = (0 to dimension - 1).filter { i => A(i, n) != 0 && (!low(i).isNegInfinity || !high(i).isPosInfinity) }
    if (indexes.isEmpty) return this
    val newA = A.copy
    val newlow = low.toDense
    val newhigh = high.toDense
    val j = indexes(0)
    val firstDel = A(j, ::)
    for (i <- indexes.drop(1)) {
      val value1 = A(j, n)
      val value2 = A(i, n)
      val currentDel = A(i, ::)
      newA(i, ::) := firstDel * (-value2) + currentDel * value1
      newlow(i) = low(j) * (-value2) + low(i) * value1
      newhigh(i) = high(j) * (-value2) + low(i) * value1
    }
    newlow(j) = -Double.NegativeInfinity
    newhigh(j) = Double.PositiveInfinity
    new Parallelotope(newlow, newA, newhigh)
  }   

  val dimension = A.numCols

  val isEmpty = false

  val isFull = low.forallValues(_.isNegInfinity) && (high.forallValues(_.isPosInfinity))

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
    val B = Aprime * (A \ DenseMatrix.eye[Double](dimension))
    val newlow = DenseVectorCol.zeros[Double](dimension)
    val newhigh = DenseVectorCol.zeros[Double](dimension)
    B.foreachNonZeroTriple {
      case (i, j, v) =>
        if (v > 0) {
          newlow(i) += v * low(i)
          newhigh(i) += v * high(i)
        } else {
          newhigh(i) += v * low(i)
          newlow(i) += v * high(i)
        }
    }
    return new Parallelotope(newlow, Aprime, newhigh)
  }

  def <=[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) ⇒ PartiallyOrdered[B]): Boolean = {
    val ptemp = this.rotate(that.A)
    val z = ptemp.low :>= that.low
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

  override def toString = "low:\n" + low + "\nhigh:\n" + high + "\nA:\n" + A
}

object Parallelotope extends NumericalDomain[Parallelotope] {
  def apply(low: DenseVectorCol[Double], A: DenseMatrix[Double], high: DenseVectorCol[Double]) =
    new Parallelotope(low, A, high)

  def full(n: Int) = {
    val low = DenseVectorCol.fill(n)(Double.NegativeInfinity)
    val high = DenseVectorCol.fill(n)(Double.PositiveInfinity)
    val A = DenseMatrix.eye[Double](n)
    new Parallelotope(low, A, high)
  }

  def empty(n: Int) = {
    throw new IllegalAccessException("Unimplemented empty ptope");
  }
}
