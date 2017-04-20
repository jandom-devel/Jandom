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

import spire.math.Rational
import spire.syntax.cfor._
import scala.language.implicitConversions

/**
  * This is a value class which implements some useful methods over arrays of rationals.
  * @param data the underlying array containing the data
  */
final class DenseVector(val data: Array[Rational]) extends AnyVal {
  /**
    * Length of the vector.
    */
  def length = data.length

  /**
    * Returns the `i`-th element of the vector.
    */
  def apply(i: Int) = data(i)

  /**
    * Updates the `i`-th element of the vector with value `v`.
    */
  def update(i: Int, v: Rational) = data.update(i, v)

  /**
    * Returns a copy of the vector.
    */
  def copy = new DenseVector(data.clone)

  /**
    * Add vector `that` to `this`.
    */
  def +=(that: DenseVector) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) += that.data(i)
    }
    this
  }

  /**
    * Subtract vector `that` from `this`.
    */
  def -=(that: DenseVector) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) -= that.data(i)
    }
    this
  }

  /**
    * Component-wise multiply `this` by `that`.
    */
  def *=(that: DenseVector) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) *= that.data(i)
    }
    this
  }

  /**
    * Component-wise divide `this` by `that`.
    */
  def /=(that: DenseVector) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) /= that.data(i)
    }
    this
  }

  /**
    * Add the rational `that` to all elements of `this`.
    */
  def +=(that: Rational) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) += that
    }
    this
  }

  /**
    * Subtract the rational `that` from all elements of `this`.
    */
  def -=(that: Rational) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) -= that
    }
    this
  }

  /**
    * Multiplies all elements of `this` by `that`.
    */
  def *=(that: Rational) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) *= that
    }
    this
  }

  /**
    * Divides all elements of `this` by `that`.
    */
  def /=(that: Rational) = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) /= that
    }
    this
  }

  /**
    * Transform `this` into its opposite.
    */
  def oppose() = {
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      data(i) = -data(i)
    }
    this
  }

  /**
    * Returns the sum of `this` and `that`.
    */
  def +(that: DenseVector) = {
    val result = this.copy
    result += that
  }

  /**
    * Returns the sum of `this` and `that`.
    */
  def -(that: DenseVector) = {
    val result = this.copy
    result -= that
  }

  /**
    * Returns the component-wise product of `this` and `that`.
    */
  def *(that: DenseVector) = {
    val result = this.copy
    result *= that
  }

  /**
    * Returns the component-wise division of `this` by `that`.
    */
  def /(that: DenseVector) = {
    val result = this.copy
    result /= that
  }

  /**
    * Returns the vector obtained by adding `that` to all elements of `this`.
    */
  def +(that: Rational) = {
    val result = this.copy
    result += that
  }

  /**
    * Returns the vector obtained by subtracting `that` from all elements of `this`.
    */
  def -(that: Rational) = {
    val result = this.copy
    result -= that
  }

  /**
    * Returns the vector obtained by multiplying all elements of `this` with `that`.
    */
  def *(that: Rational) = {
    val result = this.copy
    result *= that
  }

  /**
    * Returns the product of the column vector `this` by matrix `that`.
    */
  def *(that: DenseMatrix) = {
    val m = new DenseMatrix(data, data.length)
    m * that
  }

  /**
    * Returns the vector obtained by dividing all elements of `this` by `that`.
    */
  def /(that: Rational) = {
    val result = this.copy
    result /= that
  }

  /**
    * Returns the opposite of `this`.
    */
  def unary_- = {
    val result = this.copy
    result.oppose()
  }

  /**
    * Returns the number of non-zero elements in `this`.
    */
  def countNonZero: Int = {
    var tot = 0
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      if (data(i) != 0) tot += 1
    }
    tot
  }

  /**
    * Returns the row vector which is the transpose of `t`. The row
    * vector is represented as a dense matrix.
    */
  def t: DenseMatrix = {
    new DenseMatrix(data.clone, 1)
  }

  /**
    * It returns a bound vector obtained by applying `f` to each element
    * of `this`. The map `f` is called with box the index and the value
    * of each element.
    */
  def mapPairs(f: (Int, Rational) => RationalExt) = {
    val newdata = new Array[RationalExt](data.length)
    cfor(0)(_ < data.length, _ + 1) { (i) =>
      newdata(i) = f(i, data(i))
    }
    new Bounds(newdata)
  }

  /**
    * Returns `this` as a Scala Vector.
    */
  def toScalaVector: Vector[Rational] = Vector(data: _*)

}

/**
  * The companion object for DenseVector.
  */
object DenseVector {
  /**
    * Implicit conversion from a pair to a DenseVector.
    */
  implicit def fromTuple2(x: Tuple2[Rational, Rational]) = DenseVector(x._1, x._2)
  implicit def fromTuple3(x: Tuple3[Rational, Rational, Rational]) = DenseVector(x._1, x._2, x._3)

  /**
    * Builds a DenseVector from a sequence of rationals.
    */
  def apply(elem: Rational*): DenseVector = new DenseVector(elem.toArray)

  /**
    * Builds a DenseVector from an arrray of rationals.
    */
  def apply(data: Array[Rational]): DenseVector = new DenseVector(data)

  /**
    * Returns the null vector of dimension `n`.
    */
  def zeros(n: Int) = {
    val data = Array.fill[Rational](n)(Rational.zero)
    new DenseVector(data)
  }
}