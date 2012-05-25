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

import org.scalatest.FunSuite
import scalala.tensor.dense.DenseVector
import scalala.tensor.dense.DenseMatrix

/**
 * @author Gianluca Amato <g.amato@unich.it>
 *
 */
class ParallelotopeSuite extends FunSuite {

  val box = Parallelotope(DenseVector(-1, -1), DenseMatrix.eye(2), DenseVector(1, 1))
  
  test("constructors should only work with compatible sizes of bounds and shapes") {
    intercept[IllegalArgumentException] { Parallelotope(DenseVector(0, 2), DenseMatrix.eye(2), DenseVector(0, 2, 3)) }
  }

  test("constructors and extractors for non-trivial parallelotopes") {
    expect(2) { box.dimension }
    expect(false) { box.isEmpty }
    expect(false) { box.isFull }
  }

  test("constructors and extractors for full parallelotopes") {
    val p = Parallelotope.full(2)
    expect(2) { p.dimension }
    expect(false) { p.isEmpty }
    expect(true) { p.isFull }
  }

  test("comparison of parallelotopes") {
	val box2 = Parallelotope(DenseVector(-0.5, -0.5), DenseMatrix.eye(2), DenseVector(0.5, 0.5))
	assert ( box2<=box )
	val box3 = Parallelotope(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (-1.0, 1.0)), DenseVector(1, 1))
	assert ( box3<=box )
  }

  test("rotation of shapes") {
    val m = DenseMatrix((1.0, 1.0), (-1.0, 1.0))
    val protcalc = box.rotate(m)
    val protdef = Parallelotope(DenseVector(-2, -2), m, DenseVector(2, 2))
    expect(protdef) { protcalc }
  }
}