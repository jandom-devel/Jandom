/**
 * This file is part of JANDOM: JVM-based Analyzer for Numerical PPLBoxDoubles
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
package utils

/**
 * This is a collection of algorithms for linear algebra. It is only though to be used in Jandom.
 * For the moment, it only works with Doubles, and it is written in imperative style in order to
 * improve performance.
 */
object LinearAlgebra {

  /**
   * Computes the dot product of two vectors.
   * @param v1 the first vector
   * @param v2 the second vector
   */
  def dot(v1: Array[Double], v2: Array[Double]): Double = {
    var result = 0.0
    var i = 0
    while (i < v1.length) {
      result += v1(i) * v2(i)
      i += 1
    }
    result
  }

  /**
   * Return the product M * x
   * @param M the matrix
   * @param v the vector
   */
  def multiply(M: Array[Array[Double]], v: Array[Double]): Array[Double] = {
    val result = Array.fill(M.length)(0.0)
    var i = 0;
    while (i < M.length) {
      result(i) = dot(M(i), v)
      i += 1
    }
    result
  }

  def solve(M: Array[Array[Double]], V: Array[Array[Double]]): Array[Array[Double]] = {
    
    V
  }
}
