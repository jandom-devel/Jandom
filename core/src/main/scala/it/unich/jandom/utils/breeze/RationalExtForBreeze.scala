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
import it.unich.jandom.utils.numberext.RationalExt

/**
 * This object contains the implicit type classes which are needed to make RationalExt
 * work with the Breeze library. This is just what is strictly necessary for implementing
 * the Parallelotope domains, and might not work in a different context.
 */
object RationalExtForBreeze {

  implicit object fieldRationalExte extends Field[RationalExt] {
    def zero = RationalExt.zero

    def one = RationalExt.one

    def ==(a: RationalExt, b: RationalExt) = a == b

    def !=(a: RationalExt, b: RationalExt) = a != b

    def +(a: RationalExt, b: RationalExt) = a + b

    def -(a: RationalExt, b: RationalExt) = a - b

    def *(a: RationalExt, b: RationalExt) = a * b

    def /(a: RationalExt, b: RationalExt) = a / b

    def %(a: RationalExt, b: RationalExt) = RationalExt.zero

    def pow(a: RationalExt, b: RationalExt) = ???

    def >(a: RationalExt, b: RationalExt) = a > b

    def >=(a: RationalExt, b: RationalExt) = a >= b

    def <(a: RationalExt, b: RationalExt) = a < b

    def <=(a: RationalExt, b: RationalExt) = a <= b

    def abs(a: RationalExt) = a.abs

    implicit val normImpl: norm.Impl[RationalExt, Double] = new norm.Impl[RationalExt, Double] {
      def apply(v: RationalExt): Double = v.value.doubleValue()
    }
  }

  implicit object MulMM extends OpMulMatrix.Impl2[RationalExt, RationalExt, RationalExt] { def apply(a: RationalExt, b: RationalExt) = a * b }

  implicit object MulDM extends OpDiv.Impl2[Double, RationalExt, RationalExt] { def apply(a: Double, b: RationalExt) = RationalExt(a) * b }

  implicit object RationalExtendedIsZero extends Zero[RationalExt] {
    val zero = RationalExt.zero
  }

  implicit def dv_s_Op_RationalExtended_OpMulMatrix: OpMulMatrix.Impl2[DenseVector[RationalExt], RationalExt, DenseVector[RationalExt]] =
    new OpMulMatrix.Impl2[DenseVector[RationalExt], RationalExt, DenseVector[RationalExt]] {
      def apply(a: DenseVector[RationalExt], b: RationalExt): DenseVector[RationalExt] = {
        val ad = a.data
        var aoff = a.offset
        val result = DenseVector.zeros[RationalExt](a.length)
        val rd = result.data
        var i = 0
        while (i < a.length) {
          rd(i) = ad(aoff) * b
          aoff += a.stride
          i += 1
        }
        result
      }
      implicitly[BinaryRegistry[Vector[RationalExt], RationalExt, OpMulMatrix.type, Vector[RationalExt]]].register(this)
    }

  implicit object implOpSolveMatrixBy_DRR_DRR_eq_DRR
      extends OpSolveMatrixBy.Impl2[DenseMatrix[RationalExt], DenseMatrix[RationalExt], DenseMatrix[RationalExt]] {

    def LUSolve(X: DenseMatrix[RationalExt], A: DenseMatrix[RationalExt]) = {
      var perm = (0 until A.rows).toArray
      for (i <- 0 until A.rows - 1) {
        val optPivot = (i until A.rows) find { p => A(perm(p), i) != RationalExt.zero }
        val pivotRow = optPivot.getOrElse(throw new MatrixSingularException())
        val tmp = perm(i)
        perm(i) = perm(pivotRow)
        perm(pivotRow) = tmp
        val pivot = A(perm(i), i)
        for (j <- i + 1 until A.rows) {
          val coeff = A(perm(j), i) / pivot
          A(perm(j), ::) -= A(perm(i), ::) * coeff
          X(perm(j), ::) -= X(perm(i), ::) * coeff
        }
      }
      val X1 = new DenseMatrix[RationalExt](X.rows, X.cols)
      for (i <- A.rows - 1 to (0, -1)) {
        X1(i, ::) := X(perm(i), ::)
        for (j <- i + 1 until A.rows) {
          X1(i, ::) -= X1(j, ::) * A(perm(i), j)
        }
        X1(i, ::) /= A(perm(i), i)
      }
      X := X1.copy
    }

    override def apply(A: DenseMatrix[RationalExt], V: DenseMatrix[RationalExt]): DenseMatrix[RationalExt] = {
      require(A.rows == V.rows, "Non-conformant matrix sizes")

      if (A.size == 0) {
        DenseMatrix.zeros[RationalExt](0, 0)
      } else if (A.rows == A.cols) {
        val X = DenseMatrix.zeros[RationalExt](V.rows, V.cols)
        val Y = DenseMatrix.zeros[RationalExt](A.rows, A.cols)
        X := V
        Y := A
        LUSolve(X, Y)
        X
      } else
        throw new IllegalArgumentException("We only support solving a square matrix")
    }
  }

  implicit object implOpSolveMatrixBy_DMR_DVR_eq_DVR
      extends OpSolveMatrixBy.Impl2[DenseMatrix[RationalExt], DenseVector[RationalExt], DenseVector[RationalExt]] {

    override def apply(a: DenseMatrix[RationalExt], b: DenseVector[RationalExt]): DenseVector[RationalExt] = {
      val rv: DenseMatrix[RationalExt] = a \ new DenseMatrix[RationalExt](b.size, 1, b.data, b.offset, b.stride, true)
      new DenseVector[RationalExt](rv.data)
    }
  }

  implicit def countFromTraverseModRationalSpireExt[T](implicit traverse: CanTraverseValues[T, RationalExt]): countNonZero.Impl[T, Int] = {
    new countNonZero.Impl[T, Int] {
      def apply(t: T): Int = {
        var count: Int = 0
        traverse.traverse(t, new ValuesVisitor[RationalExt] {
          def visit(a: RationalExt) = { if (a != RationalExt.zero) count += 1 }
          def zeros(count: Int, zeroValue: RationalExt) {}
        })
        count
      }
    }
  }
}
