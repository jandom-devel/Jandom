/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import it.unich.sci.jandom.domains.CachedTopBottom

/**
 * This is the abstract domain of parallelotopes as appears in the NSAD 2012 paper. It is written
 * using the Breeze Math library. It is not safe, due to rounding problem of Arithmetic.
 * It is declared private so that we can be sure to build a single instance of it. We do not
 * directly use Scala objects because we want to keep a uniform behaviour with other
 * non-singleton abstract domains.
 *
 * @author Gianluca Amato <g.amato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
class ParallelotopeDomain private extends NumericalDomain {

  type Property = Parallelotope

  /**
   * Build a non-empty parallelotope. If the parallelotope is not empty, the result is undetermined.
   * @param A is the constraint matrix. It should be invertible.
   * @param low lower bounds.
   * @param high higher bounds.
   * @note `low` and `high` should have the same length. The  matrix `A` should be invertiible
   * of the same size of the two vectors.
   * @throws IllegalArgumentException if `low` and `high` are not of the same length, if `A` is not
   * square or if `A` has not the same size of `low`.
   */

  def apply(low: DenseVector[Double], A: DenseMatrix[Double], high: DenseVector[Double]) = {
    val isEmpty = (0 until low.size) exists { i => low(i) > high(i) }
    val isEmpty2 = (0 until low.size) exists { i => low(i).isInfinite() && low(i) == high(i) }
    new Parallelotope(isEmpty || isEmpty2, low, A, high)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def top(n: Int) = {
    val low = DenseVector.fill(n)(Double.NegativeInfinity)
    val high = DenseVector.fill(n)(Double.PositiveInfinity)
    val A = DenseMatrix.eye[Double](n)
    new Parallelotope(false, low, A, high)
    /* The full parallelotope of dimension 0 is not empty! */
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @throws $ILLEGAL
   */
  def bottom(n: Int) = {
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
  private [numerical] def extremalsInBox(lf: DenseVector[Double], low: DenseVector[Double], high: DenseVector[Double]): (Double, Double) = {
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
  private [numerical] def pivoting(m: IndexedSeq[DenseVector[Double]]): Seq[Int] = {
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
          pivots +:= (row, col)
          indexes +:= i
        case None =>
      }
      i += 1
    }
    indexes
  }
}

/**
 * Companion class for the parallelotope domain
 */
object ParallelotopeDomain {
  /**
   *  Return the parallelotope domain
   */
  def apply() = v
  private val v = new ParallelotopeDomain() with CachedTopBottom
}
