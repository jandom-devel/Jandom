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
import scalala.tensor.dense.DenseVector

/**
 * This is the abstract domain of parallelotopes.
 * @author Gianluca Amato <g.amato@unich.it>
 */
class Parallelotope(
    val isEmpty: Boolean,
    private val low: DenseVectorCol[Double],
    private val A: DenseMatrix[Double],
    private val high: DenseVectorCol[Double]) 
  extends NumericalProperty[Parallelotope] {

  require ((low.length == A.numRows) && (low.length == high.length) && A.isSquare)
  
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
      val newA = A :- (A(::,n) * (DenseVector(coeff) - ei).asRow) / coeff(n)
      new Parallelotope(false, newlow, newA, newhigh)
    } else {
      val newP = nondeterministicAssignment(n)
      val Aprime = newP.A
      val j = (Aprime(::,n) find { _ != 0 }).get       
      for ( s <- 0 to dimension -1  if Aprime(s,n) != 0 && s!=j)
        Aprime(s,::) :-= Aprime(j,::) * Aprime(s,n) / Aprime(j,n)
      val ei = SparseVector.zeros[Double](dimension)
      ei(n) = 1
      Aprime(j,::) := ei :- DenseVector(coeff)
      newP.low(j) = known
      newP.high(j) = known
      return newP
    }
  }

  def linearInequality(coeff: Array[Double], known: Double): Parallelotope = {
    val y = A.t.toDense \ DenseVector(coeff)
    val j = (0 to dimension-1) find { i => y(i)!=0 && low(i).isInfinity && high(i).isInfinity }
    j match {
      case None => {
        var minc = 0
        var maxc = 0
        for (i <- 0 to dimension-1)
          if (y(i) > 0) {
            minc += y(i) * low(i)
            maxc += y(i) * high(i)
          } else if (y(i) < 0) {
            minc += y(i) * high(i)
            maxc += y(i) * low(i)
          }       
        // val (minb,maxb) <
        val newlow = low.toDense
        val newhigh = high.toDense
        for (i <- 0 to dimension-1) {
          if (y(i)>0)
            high(i) = high(i) min (minc + y(i)*low(i))/y(i)
          else if (y(i)<0)
            low(i) = low(i) max (maxc + y(i)*low(i))/y(i)
        }
        return new Parallelotope(false, newlow, A, newhigh)
      }
      case Some(j) => {
    	val newA = A.copy
        val newhigh = high.toDense
        newA(j,::) := DenseVector(coeff)
        high(j) = known
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
    if (isEmpty) return this;
    val j = (0 to dimension - 1).filter { i => A(i, n) != 0 && (!low(i).isNegInfinity || !high(i).isPosInfinity) }
    if (j.isEmpty) return this
    val newA = A.copy
    val newlow = low.toDense
    val newhigh = high.toDense
    val j0 = j.filter { i => low(i)==high(i) }
    val j1 = j.filter { i => !low(i).isInfinity && !high(i).isInfinity }
    val r = if (! j0.isEmpty) j0(0) else if (! j1.isEmpty) j1(0) else j(0) 
    val rowr = A(r, ::)
    for (i <- j if i != r) {      
      val value1 = A(r, n)
      val value2 = A(i, n)
      val rowi = A(i, ::)
      newA(i, ::) := rowr * value2 - rowi * value1
      val (minr,maxr) = if (A(i,n) < 0) (high(r), low(r)) else (low(r), high(r))
      val (mini,maxi) = if (-A(r,n) < 0) (high(i), low(i)) else (low(i), high(i))
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
    val newlow = DenseVectorCol.zeros[Double](dimension)
    val newhigh = DenseVectorCol.zeros[Double](dimension)
    B.foreachNonZeroTriple {
      case (i, j, v) =>
        if (v > 0) {
          newlow(i) += v * low(i)
          newhigh(i) += v * high(i)
        } else if (v<0) {
          newhigh(i) += v * low(i)
          newlow(i) += v * high(i)
        } 
    }
    return new Parallelotope(false, newlow, Aprime, newhigh)
  }

  def <=[B >: Parallelotope](that: Parallelotope)(implicit arg0: (B) ⇒ PartiallyOrdered[B]): Boolean = {
    if (isEmpty) return(true)
    if (that.isEmpty) return(false)
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

  override def toString = if (isEmpty) "<empty>" else  "low:\n" + low + "\nhigh:\n" + high + "\nA:\n" + A
}

object Parallelotope extends NumericalDomain[Parallelotope] {
  
  def apply(low: DenseVectorCol[Double], A: DenseMatrix[Double], high: DenseVectorCol[Double]) =
    new Parallelotope(false, low, A, high)

  def full(n: Int) = {
    val low = DenseVectorCol.fill(n)(Double.NegativeInfinity)
    val high = DenseVectorCol.fill(n)(Double.PositiveInfinity)
    val A = DenseMatrix.eye[Double](n)
    new Parallelotope(false, low, A, high)
  }

  def empty(n: Int) = {
    val low = DenseVectorCol.fill(n)(1.0)
    val high = DenseVectorCol.fill(n)(0.0)
    val A = DenseMatrix.eye[Double](n)    
    new Parallelotope(true, low, A, high)
  }
}
