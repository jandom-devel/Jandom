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

/**
  * This is a value class which implements some useful methods over arrays of extended rationals.
  * @param data the underlying array of extended rationals.
  */
final class Bounds(val data: Array[RationalExt]) extends AnyVal {
  /**
    * Returns the length of the array.
    */
  def length = data.length

  /**
    * Returns the `i`-th element of the array.
    */
  def apply(i: Int) = data(i)

  /**
    * Returns a copy of the array.
    */
  def copy = new Bounds(data.clone)

  /**
    * Update the i-th element of the array with value `v`.
    */
  def update(i: Int, v: RationalExt) = data.update(i,v)

  /**
    * Append `that` to `this`.
    */
  def vertcat(that: Bounds) = {
    val newdata = new Array[RationalExt](data.length + that.data.length)
    Array.copy(data, 0, newdata, 0, data.length)
    Array.copy(that.data, 0, newdata, data.length, that.data.length)
    new Bounds(newdata)
  }

  /**
    * Returns a subarray of the elements in the array, whose index is in `slice`.
    */
  def apply(slice: Seq[Int]) = {
    val newdata = new Array[RationalExt](slice.length)
    for ((idx, i) <- slice.zipWithIndex) {
      newdata(i) = data(idx)
    }
    new Bounds(newdata)
  }

  /**
    * Returns true if the map `f` is true for all elements of the array.
    */
  def forall(f: RationalExt => Boolean) = data.forall(f)
}

/**
  * The compaion object of Bounds.
  */
object Bounds {
  /**
    * Create a bound vector of dimension `n` filled with `value`.
    */
  def fill(n: Int)(value: RationalExt) = new Bounds(Array.fill[RationalExt](n)(value))

  /**
    * Builds a bound vector for a sequence of extended rationals.
    */
  def apply(elem: RationalExt*) = new Bounds(elem.toArray)
}