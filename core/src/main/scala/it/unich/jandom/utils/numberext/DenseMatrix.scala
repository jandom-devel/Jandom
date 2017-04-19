/**
  * Copyright 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.utils.numberext

import spire.syntax.cfor._
import spire.math.Rational

/**
  * A DenseMatrix is a matrix of rationals. It is implemented as a single array
  * in column order.
  * @param data the underlying array containing the data
  * @param rows the number of rows in the matrix
  */
final class DenseMatrix(val data: Array[Rational], val rows: Int) {
  /**
    * The number of columns in the matrix.
    */
  val cols = if (data.length == 0 && rows == 0) 0 else data.length / rows

  /**
    * Returns the element in position (`i`,`j`).
    */
  def apply(i: Int, j: Int): Rational = data(i + j * rows)

  /**
    * Modify the element in position (`i`,`j`) with value `v`.
    */
  def update(i: Int, j: Int, v: Rational) = data.update(i + j * rows, v)

  /**
    * Returns a copy of the matrix.
    */
  def copy = new DenseMatrix(data.clone, rows)

  /**
    * Add the matrix `that` to `this`.
    */
  def +=(that: DenseMatrix): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) += that.data(i)
    }
    this
  }

  /**
    * Subtract the matrix `that` from `this`.
    */
  def -=(that: DenseMatrix): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) -= that.data(i)
    }
    this
  }

  /**
    * Returns the sum of `this` and `that`.
    */
  def +(that: DenseMatrix) = {
    val result = this.copy
    result += that
  }

  /**
    * Returns the `this` -  `that`.
    */
  def -(that: DenseMatrix) = {
    val result = this.copy
    result -= that
  }

  /**
    * Sum the rational `that` to all elements in the matrix.
    */
  def +=(that: Rational): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) += that
    }
    this
  }

  /**
    * Returns the matrix obtained by summing `that` to all elements in `this`.
    */
  def +(that: Rational) = {
    val result = this.copy
    result += that
  }

  /**
    * Sum the rational `that` from all elements in the matrix.
    */
  def -=(that: Rational): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) -= that
    }
    this
  }

  /**
    * Returns the matrix obtained by subtracting `that` from all elements in `this`.
    */
  def -(that: Rational) = {
    val result = this.copy
    result -= that
  }

  /**
    * Multiplies all the elements in the matrix by `that`
    */
  def *=(that: Rational): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) *= that
    }
    this
  }

  /**
    * Returns the matrix obtained by multipling all elements in `this` by `that`.
    */
  def *(that: Rational) = {
    val result = this.copy
    result *= that
  }

  /**
    * Divides all elements in the matrix by `that`
    */
  def /=(that: Rational): DenseMatrix = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) /= that
    }
    this
  }

  /**
    * Returns the matrix obtained by dividing all elements in `this` by `that`.
    */
  def /(that: Rational) = {
    val result = this.copy
    result /= that
  }

  /**
    * Returns the row-by-column product of `this` and `that`.
    */
  def *(that: DenseMatrix): DenseMatrix = {
    require(this.cols == that.rows)
    var result = DenseMatrix.raw(this.rows, that.cols)
    cfor(0)(_ < rows, _ + 1) { (i) =>
      cfor(0)(_ < that.cols, _ + 1) { (j) =>
        result(i, j) = Rational(0)
        cfor(0)(_ < cols, _ + 1) { (k) =>
          result(i, j) += this(i, k) * that(k, j)
        }
      }
    }
    result
  }

  /**
    * Returns a matrix `A` such that `this * A = that`.
    */
  def \(that: DenseMatrix): DenseMatrix = {
    require(this.rows == that.rows, "Non-conformant matrices size")
    if (data.size == 0)
      DenseMatrix.zeros(0, 0)
    else {
      val A = data.clone
      val X = that.data.clone
      val Y = DenseMatrix.LUSolveArray(X, A, that.rows, that.cols)
      new DenseMatrix(Y, rows)
    }
  }

  /**
    * Returns a vector `v` such that `this * v = that`.
    */
  def \(that: DenseVector): DenseVector = {
    val M = new DenseMatrix(that.data, that.length)
    val res = this \ M
    new DenseVector(res.data)
  }

  /**
    * Returns the transpose of `this`.
    */
  def t: DenseMatrix = {
    val result = new DenseMatrix(new Array[Rational](data.length), cols)
    cfor(0)(_ < rows, _ + 1) { (i) =>
      cfor(0)(_ < cols, _ + 1) { (j) =>
        result(j, i) = this(i, j)
      }
    }
    result
  }

  /**
    * Horizontally concatenates `that` to `this`. The two matrices are required to have the same number
    * of rows, and the columns of `that` are added as additional columns to  `this`.
    */
  def horzcat(that: DenseMatrix) = {
    require(rows == that.rows)
    val result = new DenseMatrix(new Array[Rational](this.rows * (this.cols + that.cols)), rows)
    Array.copy(this.data, 0, result.data, 0, data.length)
    Array.copy(that.data, 0, result.data, data.length, that.data.length)
    result
  }

  /**
    * Vertically concatenates `that` to `this`. The two matrices are required to have the same number
    * of columns, and the rows of `that` are added as additional rows to `this`.
    */
  def vertcat(that: DenseMatrix) = {
    require(cols == that.cols)
    val result = new DenseMatrix(new Array[Rational]((this.rows + that.rows) * this.cols), this.rows + that.rows)
    cfor(0)(_ < cols, _ + 1) { (j) =>
      cfor(0)(_ < this.rows, _ + 1) { (i) =>
        result(i, j) = this(i, j)
      }
      cfor(0)(_ < that.rows, _ + 1) { (i) =>
        result(i + rows, j) = that(i, j)
      }
    }
    result
  }

  /**
    * Returns the `i`-th row of `this` as a vector.
    */
  def row(i: Int): DenseVector = {
    val data = new Array[Rational](cols)
    cfor(0)(_ < cols, _ + 1) { (j) =>
      data(j) = this(i, j)
    }
    new DenseVector(data)
  }

  /**
    * Replaces the `i`-th row of `this` with `v`.
    */
  def rowUpdate(i: Int, v: DenseVector): Unit = {
    cfor(0)(_ < cols, _ + 1) { (j) =>
      this(i,j) = v(j)
    }
  }

  /**
    * Returns the `i`-th column of `this` as a vector.
    */
  def col(j: Int) = {
    val data = new Array[Rational](rows)
    cfor(0)(_ < rows, _ + 1) { (i) =>
      data(i) = this(i, j)
    }
    new DenseVector(data)
  }

  /**
    * Returns an array `a` such that `a(i)` contains the number of non-zero elements
    * in the `i`-th row of `this`.
    */
  def countNonZeroInRows: Array[Int] = {
    val res = new Array[Int](rows)
    var tot = 0
    cfor(0) (_ < rows, _ +1 ) { (i) =>
      tot = 0
      cfor(0) ( _ < cols, _ + 1) { (j) =>
        if (this(i,j) != 0) tot += 1
      }
      res(i) = tot
    }
    res
  }

  /**
    * Returns a submatrix of `this`.
    * @param slicer the sequence of rows to extract from `this`
    * @param slicec the sequence of columns to extract from `this`
    * @return the resulting submatrix
    */
  def apply(slicer: Seq[Int], slicec: Seq[Int]): DenseMatrix = {
    val result = new DenseMatrix(new Array[Rational](slicer.length*slicec.length), slicer.length)
    for ((i, idxi) <- slicer.zipWithIndex; (j, idxj) <- slicec.zipWithIndex) {
      result(idxi,idxj) = this(i, j)
    }
    result
  }

  /**
    * For each position `(i,j)` in `this`, `f` is called with parameters
    * `(i,j)` and `this((i,j))`.
    */
  def foreachPair(f: ((Int, Int), Rational) => Unit): Unit = {
    cfor (0)(_ < rows, _ +1) { (i) =>
      cfor(0) ( _ < cols, _ +1 ) { (j) =>
        f((i,j), this(i,j))
      }
    }
  }

  override def toString = {
    val sb = new StringBuilder
    cfor (0)(_ < rows, _ + 1) { (i) =>
      cfor (0)(_ < cols, _ + 1) { (j) =>
        sb ++= this(i,j).toString
        sb += ' '
      }
      sb += '\n'
    }
    sb.toString
  }
}

/**
  * The compaion object for DenseMatrix
  */
object DenseMatrix {

  /**
    * Returns a matrix with `n` rows and `m` columns. Elements of the matrix
    * are not initialized.
    */
  def raw(n: Int, m: Int) = new DenseMatrix(new Array[Rational](n * m), n)

  /**
    * Returns a diagonal matrix of order `n`.
    */
  def eye(n: Int) = {
    val data = new Array[Rational](n * n)
    cfor(0)(_ < n, _ + 1) { (i) =>
      cfor(0)(_ < n, _ + 1) { (j) =>
        data(i + j * n) = if (i == j) Rational.one else Rational.zero
      }
    }
    new DenseMatrix(data, n)
  }

  /**
    * Returns a matrix with `n` rows and `m` columns filled with zero.
    */
  def zeros(n: Int, m: Int) = {
    val data = Array.fill[Rational](m * n)(Rational.zero)
    new DenseMatrix(data, n)
  }

  /**
    * Build a matrix from a sequence of vectors. Each vector will be a row
    * in the resulting matrix.
    */
  def apply(rows: DenseVector*) = {
    if (rows.isEmpty)
      new DenseMatrix(new Array[Rational](0), 0)
    else {
      val cols = rows(0).length
      val result = new DenseMatrix(new Array[Rational](cols * rows.length), rows.length)
      for ((row, i) <- rows.zipWithIndex) {
        cfor(0)(_ < cols, _ + 1) { (j) =>
          result(i, j) = row(j)
        }
      }
      result
    }
  }

  /**
    * An helper private method which implements Gaussian elimination.
    */
  private def LUSolveArray(X: Array[Rational], A: Array[Rational], Xrows: Int, Xcols: Int): Array[Rational] = {
    val perm = (0 until Xrows).toArray
    for (i <- 0 until Xrows - 1) {
      val optPivot = (i until Xrows) find { p => !A(perm(p) + i * Xrows).isZero }
      val pivotRow = optPivot.getOrElse(throw new IllegalArgumentException("Non invertible matrix"))
      val tmp = perm(i)
      perm(i) = perm(pivotRow)
      perm(pivotRow) = tmp
      val pivot = A(perm(i) + i * Xrows)
      for (j <- i + 1 until Xrows) {
        val coeff = A(perm(j) + i * Xrows) / pivot
        cfor(0)(_ < Xrows, _ + 1) { (k) =>
          A(perm(j) + k * Xrows) -= A(perm(i) + k * Xrows) * coeff
        }
        cfor(0)(_ < Xcols, _ + 1) { (k) =>
          X(perm(j) + k * Xrows) -= X(perm(i) + k * Xrows) * coeff
        }
      }
    }
    val X1 = new Array[Rational](Xrows * Xcols)
    for (i <- Xrows - 1 to (0, -1)) {
      cfor(0)(_ < Xcols, _ + 1) { (k) =>
        X1(i + k * Xrows) = X(perm(i) + k * Xrows)
      }
      for (j <- i + 1 until Xrows)
        cfor(0)(_ < Xcols, _ + 1) { (k) =>
          X1(i + k * Xrows) -= X1(j + k * Xrows) * A(perm(i) + j * Xrows)
        }
      cfor(0)(_ < Xcols, _ + 1) { (k) =>
        X1(i + k * Xrows) /= A(perm(i) + i * Xrows)
      }
    }
    X1
  }
}
