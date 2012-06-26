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
  val diamond = Parallelotope(DenseVector(-1, -1), DenseMatrix((1.0,1.0),(1.0,-1.0)), DenseVector(1, 1))
  val empty = Parallelotope.empty(2)
  val full = Parallelotope.full(2)
  
  test("constructors should only work with compatible sizes of bounds and shapes") {
    intercept[IllegalArgumentException] { Parallelotope(DenseVector(0, 2), DenseMatrix.eye(2), DenseVector(0, 2, 3)) }
  }

  test("constructors and extractors for non-trivial parallelotopes") {
    expect(2) { box.dimension }
    expect(false) { box.isEmpty }
    expect(false) { box.isFull }
  }

  test("constructors and extractors for full parallelotopes") {    
    expect(2) { full.dimension }
    expect(false) { full.isEmpty }
    expect(true) { full.isFull }
  }
  
  test("constructors and extractors for empty parallelotopes") {
    expect(2) { empty.dimension }
    expect(true) { empty.isEmpty }
    expect(false) { empty.isFull }
  }
   
  test("comparison of parallelotopes") {
    assert (empty < box)
    assert (box < full)
    assert (empty < full)
    assert (diamond < box)
    assert (diamond <= box)
    assert (box > diamond)
    assert (box >= diamond)    
 	expect ( Some(1) ) { box.tryCompareTo(diamond) }
    expect ( Some(-1) ) { diamond.tryCompareTo(box) }
    assert (box == box)
    expect ( Some(0) ) { box.tryCompareTo(box) }
	val box2 = Parallelotope(DenseVector(-0.5, -0.5), DenseMatrix.eye(2), DenseVector(0.5, 0.5))
	assert ( box2<=box )
	assert ( box>=box2)
	assert ( box2<box )
	assert ( box>box2 )
    val box3 = Parallelotope(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(2, 2))
    expect ( None ) { box.tryCompareTo(box3) }
	val r1 = Parallelotope(DenseVector(0,0), DenseMatrix.eye(2), DenseVector(0, Double.PositiveInfinity))
    val r2 = Parallelotope(DenseVector(0,Double.NegativeInfinity), DenseMatrix((1.0,0.0),(1.0,-1.0)), DenseVector(0,0))
    expect(r2) { r1 }
  }

  test("rotation of shapes") {
    val m = DenseMatrix((1.0, 1.0), (-1.0, 1.0))
    val protcalc = box.rotate(m)
    val protdef = Parallelotope(DenseVector(-2, -2), m, DenseVector(2, 2))
    expect(protdef) { protcalc }
  }

  test("linear invertible assignment") {
    val li1 = Parallelotope(DenseVector(0,-1), DenseMatrix((1.0, -1.0),(0.0 ,1.0)), DenseVector(2,1))
    expect (li1) { box.linearAssignment(0,Array(1,1),1) }
    val li2 = Parallelotope(DenseVector(1,-1), DenseMatrix((1.0, 0.0),(-1.0 ,1.0)), DenseVector(1,0))
    val li3 = Parallelotope(DenseVector(2,-2), DenseMatrix((1.0, 0.0),(-1.0 ,1.0)), DenseVector(2,-1))
    expect (li3) { li2.linearAssignment(0,Array(1,0),1) }
    val li4 = Parallelotope(DenseVector(-1,-2),DenseMatrix((1.0,0.0),(-1.0,1.0)),DenseVector(1,2))
    expect (li4) { box.linearAssignment(1,Array(1,2),0) }
  }
  
  test("non-invertible linear assignment") {
    val ln1 = Parallelotope(DenseVector(2,-1), DenseMatrix((1.0,-1.0),(0.0,1.0)), DenseVector(2,1))
    expect (ln1) { box.linearAssignment(0, Array(0,1), 2)}
    val ln2 = Parallelotope(DenseVector(0,Double.NegativeInfinity), DenseMatrix((-1.0,1.0),(0.0,1.0)), DenseVector(0,Double.PositiveInfinity))
    val ln3 = Parallelotope(DenseVector(Double.NegativeInfinity,0), DenseMatrix((1.0,-1.0),(0.0,1.0)), DenseVector(Double.PositiveInfinity,0))
    expect (ln2) { ln3.linearAssignment(1, Array(1,0), 0)}
  }
  
  test("non-deterministic assignment") {
    val nd1 = Parallelotope(DenseVector(Double.NegativeInfinity,-1), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity,1))
    expect (nd1) { box.nondeterministicAssignment(0) }
    expect (nd1) { nd1.nondeterministicAssignment(0) }
    expect (nd1) { diamond.nondeterministicAssignment(0) }
    val nd2 = Parallelotope(DenseVector(0,0), DenseMatrix((2.0,1.0),(2.0,-1.0)), DenseVector(1,1))
    val nd3 = Parallelotope(DenseVector(Double.NegativeInfinity,-1), DenseMatrix((2.0,1.0),(0.0,-2.0)), DenseVector(Double.PositiveInfinity,1))
    expect (nd3) { nd2.nondeterministicAssignment(0) }
    val nd4 = Parallelotope(DenseVector(Double.NegativeInfinity,0), DenseMatrix((2.0,1.0),(4.0,0.0)), DenseVector(Double.PositiveInfinity,2))
    expect (nd4) { nd2.nondeterministicAssignment(1) }
    val nd5 = Parallelotope(DenseVector(10,-1), DenseMatrix((1.0,0.0),(1.0,1.0)), DenseVector(10,1))
    val nd6 = Parallelotope(DenseVector(Double.NegativeInfinity,-11), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity,-9))
    expect(nd6) { nd5.nondeterministicAssignment(0) }
  }

  test("linear inequalities") {
    val li1 = Parallelotope(DenseVector(-1,-1), DenseMatrix((1.0,1.0),(1.0,-1.0)), DenseVector(0,0))
    expect(li1) { diamond.linearInequality(Array(2,0), -1) }
    val li2 = Parallelotope(DenseVector(-1,Double.NegativeInfinity), DenseMatrix((1.0,1.0),(1.0,-1.0)), DenseVector(1,Double.PositiveInfinity))
    val li3 = Parallelotope(DenseVector(-1,Double.NegativeInfinity), DenseMatrix((1.0,1.0),(1.0,0.0)), DenseVector(1,-2))
    expect(li3) { li2.linearInequality(Array(1,0),-2) }
    val quadrant = Parallelotope(DenseVector(0,0), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity, Double.PositiveInfinity))
    expect(quadrant) { quadrant.linearInequality(Array(1,-1),0) }
    val li4 = Parallelotope(DenseVector(10,-10), DenseMatrix.eye(2), DenseVector(10,-10))
    expect (empty) { li4.linearInequality(Array(1,-1),0) }
  }
  
}