/**
 * Copyright 2013 Gianluca Amato
 * 
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
 */

package it.unich.sci.jandom.domains

import org.scalatest.FunSuite

import breeze.linalg._

/**
 * Test suite for the parallelotope domain. Disabled at the moment due to non-functionin domain.
 * @author Gianluca Amato <g.amato@unich.it>
 *
 */
class ParallelotopeSuite extends FunSuite {

  val box = Parallelotope(DenseVector(-1, -1), DenseMatrix.eye(2), DenseVector(1, 1))
  val diamond = Parallelotope(DenseVector(-1, -1), DenseMatrix((1.0,1.0),(1.0,-1.0)), DenseVector(1, 1))
  val empty = Parallelotope.empty(2)
  val full= Parallelotope.full(2)
  
  test("constructors should only work with compatible sizes of bounds and shapes") {
    intercept[IllegalArgumentException] { Parallelotope(DenseVector(0, 2), DenseMatrix.eye(2), DenseVector(0, 2, 3)) }
  }

  test("constructors and extractors for non-trivial parallelotopes") {
    expectResult(2) { box.dimension }
    expectResult(false) { box.isEmpty }
    expectResult(false) { box.isFull }
  }

  test("constructors and extractors for full parallelotopes") {    
    expectResult(2) { full.dimension }
    expectResult(false) { full.isEmpty }
    expectResult(true) { full.isFull }
  }
  
  test("constructors and extractors for empty parallelotopes") {
    expectResult(2) { empty.dimension }
    expectResult(true) { empty.isEmpty }
    expectResult(false) { empty.isFull }
  }
   
  test("comparison of parallelotopes") {
    assert (empty < box)
    assert (box < full)
    assert (empty < full)
    assert (diamond < box)
    assert (diamond <= box)
    assert (box > diamond)
    assert (box >= diamond)    
 	expectResult ( Some(1) ) { box.tryCompareTo(diamond) }
    expectResult ( Some(-1) ) { diamond.tryCompareTo(box) }
    assert (box == box)
    expectResult ( Some(0) ) { box.tryCompareTo(box) }
	val box2 = Parallelotope(DenseVector(-0.5, -0.5), DenseMatrix.eye(2), DenseVector(0.5, 0.5))
	assert ( box2<=box )
	assert ( box>=box2)
	assert ( box2<box )
	assert ( box>box2 )
    val box3 = Parallelotope(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(2, 2))
    expectResult ( None ) { box.tryCompareTo(box3) }
  }

  test("rotation of shapes") {
    val m = DenseMatrix((1.0, 1.0), (-1.0, 1.0))
    val protcalc = box.rotate(m)
    val protdef = Parallelotope(DenseVector(-2, -2), m, DenseVector(2, 2))
    expectResult(protdef) { protcalc }
  }

  test("linear invertible assignment") {
    val li1 = Parallelotope(DenseVector(0,-1), DenseMatrix((1.0, -1.0),(0.0 ,1.0)), DenseVector(2,1))
    expectResult (li1) { box.linearAssignment(0,Array(1,1),1) }
    val li2 = Parallelotope(DenseVector(1,-1), DenseMatrix((1.0, 0.0),(-1.0 ,1.0)), DenseVector(1,0))
    val li3 = Parallelotope(DenseVector(2,-2), DenseMatrix((1.0, 0.0),(-1.0 ,1.0)), DenseVector(2,-1))
    expectResult (li3) { li2.linearAssignment(0,Array(1,0),1) }
    expectResult (li3) { li2.linearAssignment(0,Array(1),1) }
    val li4 = Parallelotope(DenseVector(-1,-2),DenseMatrix((1.0,0.0),(-1.0,1.0)),DenseVector(1,2))
    expectResult (li4) { box.linearAssignment(1,Array(1,2),0) }
    assert(empty.linearAssignment(1, Array(1,1), 0).isEmpty)
  }
  
  test("non-invertible linear assignment") {
    val ln1 = Parallelotope(DenseVector(2,-1), DenseMatrix((1.0,-1.0),(0.0,1.0)), DenseVector(2,1))
    expectResult (ln1) { box.linearAssignment(0, Array(0,1), 2)}
    val ln2 = Parallelotope(DenseVector(0,Double.NegativeInfinity), DenseMatrix((-1.0,1.0),(0.0,1.0)), DenseVector(0,Double.PositiveInfinity))
    val ln3 = Parallelotope(DenseVector(Double.NegativeInfinity,0), DenseMatrix((1.0,-1.0),(0.0,1.0)), DenseVector(Double.PositiveInfinity,0))
    expectResult (ln2) { ln3.linearAssignment(1, Array(1,0), 0)}
    expectResult (ln2) { ln3.linearAssignment(1, Array(1), 0)}
    assert(empty.linearAssignment(1, Array(1,0), 0).isEmpty)
  }
  
  test("non-deterministic assignment") {
    val nd1 = Parallelotope(DenseVector(Double.NegativeInfinity,-1), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity,1))
    expectResult (nd1) { box.nonDeterministicAssignment(0) }
    expectResult (nd1) { nd1.nonDeterministicAssignment(0) }
    expectResult (nd1) { diamond.nonDeterministicAssignment(0) }
    val nd2 = Parallelotope(DenseVector(0,0), DenseMatrix((2.0,1.0),(2.0,-1.0)), DenseVector(1,1))
    val nd3 = Parallelotope(DenseVector(Double.NegativeInfinity,-1), DenseMatrix((2.0,1.0),(0.0,-2.0)), DenseVector(Double.PositiveInfinity,1))
    expectResult (nd3) { nd2.nonDeterministicAssignment(0) }
    val nd4 = Parallelotope(DenseVector(Double.NegativeInfinity,0), DenseMatrix((2.0,1.0),(4.0,0.0)), DenseVector(Double.PositiveInfinity,2))
    expectResult (nd4) { nd2.nonDeterministicAssignment(1) }
    val nd5 = Parallelotope(DenseVector(10,-1), DenseMatrix((1.0,0.0),(1.0,1.0)), DenseVector(10,1))
    val nd6 = Parallelotope(DenseVector(Double.NegativeInfinity,-11), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity,-9))
    expectResult(nd6) { nd5.nonDeterministicAssignment(0) }
    assert(empty.nonDeterministicAssignment(0).isEmpty)
  }

  test("linear inequalities") {
    val li1 = Parallelotope(DenseVector(-1,-1), DenseMatrix((1.0,1.0),(1.0,-1.0)), DenseVector(0,0))
    expectResult(li1) { diamond.linearInequality(Array(2,0), 1) }
    expectResult(li1) { diamond.linearInequality(Array(2), 1) }
    assert(empty.linearInequality(Array(1,0),-1).isEmpty)
  }
  
  test("union") {		
    val u1 = Parallelotope(DenseVector(2,0), DenseMatrix.eye(2), DenseVector(4,2))
    val u2 = Parallelotope(DenseVector(-4,-1), DenseMatrix((-1.0,3.0),(0.0,1.0)), DenseVector(4,2))
    expectResult(u2) { box union u1 }
    val u3 = Parallelotope(DenseVector(-1,-1), DenseMatrix((0.0,1.0),(1.0,-1.0)), DenseVector(2,4))
    expectResult(u3) { u1 union diamond }
    val u4 = Parallelotope(DenseVector(-4,0), DenseMatrix.eye(2), DenseVector(-2,2))
    val u5 = Parallelotope(DenseVector(-4,0), DenseMatrix.eye(2), DenseVector(4,2))
    expectResult (u5) { u4 union u1 }
    val u6 = Parallelotope(DenseVector(1, Double.NegativeInfinity), DenseMatrix((1.0,0.0),(1.0,-1.0)), DenseVector(1,1))
    val u7 = Parallelotope(DenseVector(0, Double.NegativeInfinity), DenseMatrix((1.0,0.0),(0.0,-1.0)), DenseVector(0,0))
    val u8 = Parallelotope(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(1,Double.PositiveInfinity))
    expectResult (u8) { u6 union u7 }
    val u9 =  Parallelotope(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(0,Double.PositiveInfinity))
    expectResult (u8) { u9 union u8 }
    expectResult (u8) { u8 union u9 }    
    val u10 = Parallelotope(DenseVector(2,0), DenseMatrix.eye(2), DenseVector(2,0))
    val u11 = Parallelotope(DenseVector(0,2), DenseMatrix((0.0,1.0),(1.0,-2.0)), DenseVector(1,6))
    expectResult (u11) { u10 union u11 }
  }
  
  test("string representation") {
    expectResult(Seq("-1.0 <= x+y <= 1.0", "-1.0 <= x-y <= 1.0")) { diamond.mkString(IndexedSeq("x","y")) } 
    expectResult("[ empty ]") { empty.toString }
    expectResult("[ -Infinity <= v0 <= Infinity , -Infinity <= v1 <= Infinity ]") { full.toString }    
  }
  
}
