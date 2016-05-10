/**
 * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.utils.breeze

import breeze.linalg._
import breeze.linalg.operators._
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.math.Field
import breeze.storage.Zero
import spire.math.Rational
import spire.syntax.cfor._

/**
 * This object contains the implicit type classes which are needed to make Rational
 * work with the Breeze library. This is just what is strictly necessary for implementing
 * the ParallelotopeRational domain, and might not work in a different context.
 */
object RationalForBreeze {

  implicit object fieldRational extends Field[Rational] {
    def zero = Rational.zero

    def one = Rational.one

    def ==(a: Rational, b: Rational) = a == b

    def !=(a: Rational, b: Rational) = a != b

    def +(a: Rational, b: Rational) = a + b

    def -(a: Rational, b: Rational) = a - b

    def *(a: Rational, b: Rational) = a * b

    def /(a: Rational, b: Rational) = a / b

    def %(a: Rational, b: Rational) = Rational.zero

    def pow(a: Rational, b: Rational) = ???

    def >(a: Rational, b: Rational) = a > b

    def >=(a: Rational, b: Rational) = a >= b

    def <(a: Rational, b: Rational) = a < b

    def <=(a: Rational, b: Rational) = a <= b

    def abs(a: Rational) = a.abs

    implicit val normImpl: norm.Impl[Rational, Double] = new norm.Impl[Rational, Double] {
      def apply(v: Rational): Double = v.doubleValue()
    }
  }

  implicit object Rational_MulMM extends OpMulMatrix.Impl2[Rational, Rational, Rational] { def apply(a: Rational, b: Rational) = a * b }

  implicit object Rational_MulDM extends OpDiv.Impl2[Double, Rational, Rational] { def apply(a: Double, b: Rational) = Rational(a) * b }

  implicit object RationalIsZero extends Zero[Rational] {
    val zero = Rational.zero
  }

  implicit def dv_s_Op_Rational_OpMulMatrix: OpMulMatrix.Impl2[DenseVector[Rational], Rational, DenseVector[Rational]] =
    new OpMulMatrix.Impl2[DenseVector[Rational], Rational, DenseVector[Rational]] {
      def apply(a: DenseVector[Rational], b: Rational): DenseVector[Rational] = {
        val ad = a.data
        var aoff = a.offset
        val result = DenseVector.zeros[Rational](a.length)
        val rd = result.data
        var i = 0
        while (i < a.length) {
          rd(i) = ad(aoff) * b
          aoff += a.stride
          i += 1
        }
        result
      }
      implicitly[BinaryRegistry[Vector[Rational], Rational, OpMulMatrix.type, Vector[Rational]]].register(this)
    }

  implicit object Rational_implOpSolveMatrixBy_DRR_DRR_eq_DRR
      extends OpSolveMatrixBy.Impl2[DenseMatrix[Rational], DenseMatrix[Rational], DenseMatrix[Rational]] {

    def LUSolveArray(X: Array[Rational], A: Array[Rational], Xrows: Int, Xcols: Int): Array[Rational] = {
      val perm = (0 until Xrows).toArray
      for (i <- 0 until Xrows - 1) {
        val optPivot = (i until Xrows) find { p => !A(perm(p) + i * Xrows).isZero }
        val pivotRow = optPivot.getOrElse(throw new MatrixSingularException())
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

    def apply(A: DenseMatrix[Rational], V: DenseMatrix[Rational]): DenseMatrix[Rational] = {
      require(A.rows == V.rows, "Non-conformant matrix sizes")

      if (A.size == 0) {
        DenseMatrix.zeros[Rational](0, 0)
      } else if (A.rows == A.cols) {
        val X = DenseMatrix.zeros[Rational](V.rows, V.cols)
        val Y = DenseMatrix.zeros[Rational](A.rows, A.cols)
        X := V
        Y := A
        new DenseMatrix(X.rows, X.cols, LUSolveArray(X.data, Y.data, X.rows, X.cols), 0, X.rows)
      } else
        throw new IllegalArgumentException("We only support solving a square matrix")
    }
  }

  implicit object Rational_implOpSolveMatrixBy_DMR_DVR_eq_DVR
      extends OpSolveMatrixBy.Impl2[DenseMatrix[Rational], DenseVector[Rational], DenseVector[Rational]] {

    def apply(a: DenseMatrix[Rational], b: DenseVector[Rational]): DenseVector[Rational] = {
      val rv: DenseMatrix[Rational] = a \ new DenseMatrix[Rational](b.size, 1, b.data, b.offset, b.stride, true)
      new DenseVector[Rational](rv.data)
    }
  }

  implicit def countFromTraverseModRational[T](implicit traverse: CanTraverseValues[T, Rational]): countNonZero.Impl[T, Int] = {
    new countNonZero.Impl[T, Int] {
      def apply(t: T): Int = {
        var count: Int = 0
        traverse.traverse(t, new ValuesVisitor[Rational] {
          def visit(a: Rational) = { if (a != Rational.zero) count += 1 }
          def zeros(count: Int, zeroValue: Rational) {}
        })
        count
      }
    }
  }
}
