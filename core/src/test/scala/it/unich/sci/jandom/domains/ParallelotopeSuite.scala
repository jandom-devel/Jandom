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
import it.unich.sci.jandom.domains.numerical.ParallelotopeDomain
import breeze.linalg._
import it.unich.sci.jandom.domains.numerical.LinearForm

/**
 * Test suite for the parallelotope domain. Disabled at the moment due to non-functionin domain.
 * @author Gianluca Amato <g.amato@unich.it>
 *
 */
class ParallelotopeSuite extends FunSuite {
  val Parallelotope = ParallelotopeDomain()

  val box = Parallelotope(DenseVector(-1, -1), DenseMatrix.eye(2), DenseVector(1, 1))
  val diamond = Parallelotope(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (1.0, -1.0)), DenseVector(1, 1))
  val empty = Parallelotope.bottom(2)
  val full = Parallelotope.top(2)

  test("constructors should only work with compatible sizes of bounds and shapes") {
    intercept[IllegalArgumentException] { Parallelotope(DenseVector(0, 2), DenseMatrix.eye(2), DenseVector(0, 2, 3)) }
  }

  test("constructors and extractors for non-trivial parallelotopes") {
    assertResult(2) { box.dimension }
    assertResult(false) { box.isEmpty }
    assertResult(false) { box.isTop }
  }

  test("constructors and extractors for full parallelotopes") {
    assertResult(2) { full.dimension }
    assertResult(false) { full.isEmpty }
    assertResult(true) { full.isTop }
  }

  test("constructors and extractors for empty parallelotopes") {
    assertResult(2) { empty.dimension }
    assertResult(true) { empty.isEmpty }
    assertResult(false) { empty.isTop }
  }

  test("comparison of parallelotopes") {
    assert(empty < box)
    assert(box < full)
    assert(empty < full)
    assert(diamond < box)
    assert(diamond <= box)
    assert(box > diamond)
    assert(box >= diamond)
    assertResult(Some(1)) { box.tryCompareTo(diamond) }
    assertResult(Some(-1)) { diamond.tryCompareTo(box) }
    assert(box == box)
    assertResult(Some(0)) { box.tryCompareTo(box) }
    val box2 = Parallelotope(DenseVector(-0.5, -0.5), DenseMatrix.eye(2), DenseVector(0.5, 0.5))
    assert(box2 <= box)
    assert(box >= box2)
    assert(box2 < box)
    assert(box > box2)
    val box3 = Parallelotope(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(2, 2))
    assertResult(None) { box.tryCompareTo(box3) }
  }

  test("rotation of shapes") {
    val m = DenseMatrix((1.0, 1.0), (-1.0, 1.0))
    val protcalc = box.rotate(m)
    val protdef = Parallelotope(DenseVector(-2, -2), m, DenseVector(2, 2))
    assertResult(protdef) { protcalc }
  }

  test("linear invertible assignment") {
    val li1 = Parallelotope(DenseVector(0, -1), DenseMatrix((1.0, -1.0), (0.0, 1.0)), DenseVector(2, 1))
    assertResult(li1) { box.linearAssignment(0, LinearForm(1.0, 1, 1)) }
    val li2 = Parallelotope(DenseVector(1, -1), DenseMatrix((1.0, 0.0), (-1.0, 1.0)), DenseVector(1, 0))
    val li3 = Parallelotope(DenseVector(2, -2), DenseMatrix((1.0, 0.0), (-1.0, 1.0)), DenseVector(2, -1))
    assertResult(li3) { li2.linearAssignment(0, LinearForm(1.0, 1, 0)) }
    assertResult(li3) { li2.linearAssignment(0, LinearForm(1.0, 1)) }
    val li4 = Parallelotope(DenseVector(-1, -2), DenseMatrix((1.0, 0.0), (-1.0, 1.0)), DenseVector(1, 2))
    assertResult(li4) { box.linearAssignment(1, LinearForm(0.0, 1, 2)) }
    assert(empty.linearAssignment(1, LinearForm(0.0, 1, 1)).isEmpty)
  }

  test("non-invertible linear assignment") {
    val ln1 = Parallelotope(DenseVector(2, -1), DenseMatrix((1.0, -1.0), (0.0, 1.0)), DenseVector(2, 1))
    assertResult(ln1) { box.linearAssignment(0, LinearForm(2.0, 0, 1)) }
    val ln2 = Parallelotope(DenseVector(0, Double.NegativeInfinity), DenseMatrix((-1.0, 1.0), (0.0, 1.0)), DenseVector(0, Double.PositiveInfinity))
    val ln3 = Parallelotope(DenseVector(Double.NegativeInfinity, 0), DenseMatrix((1.0, -1.0), (0.0, 1.0)), DenseVector(Double.PositiveInfinity, 0))
    assertResult(ln2) { ln3.linearAssignment(1, LinearForm(0.0, 1, 0)) }
    assertResult(ln2) { ln3.linearAssignment(1, LinearForm(0.0, 1)) }
    assert(empty.linearAssignment(1, LinearForm(0.0, 1, 0)).isEmpty)
  }

  test("non-deterministic assignment") {
    val nd1 = Parallelotope(DenseVector(Double.NegativeInfinity, -1), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity, 1))
    assertResult(nd1) { box.nonDeterministicAssignment(0) }
    assertResult(nd1) { nd1.nonDeterministicAssignment(0) }
    assertResult(nd1) { diamond.nonDeterministicAssignment(0) }
    val nd2 = Parallelotope(DenseVector(0, 0), DenseMatrix((2.0, 1.0), (2.0, -1.0)), DenseVector(1, 1))
    val nd3 = Parallelotope(DenseVector(Double.NegativeInfinity, -1), DenseMatrix((2.0, 1.0), (0.0, -2.0)), DenseVector(Double.PositiveInfinity, 1))
    assertResult(nd3) { nd2.nonDeterministicAssignment(0) }
    val nd4 = Parallelotope(DenseVector(Double.NegativeInfinity, 0), DenseMatrix((2.0, 1.0), (4.0, 0.0)), DenseVector(Double.PositiveInfinity, 2))
    assertResult(nd4) { nd2.nonDeterministicAssignment(1) }
    val nd5 = Parallelotope(DenseVector(10, -1), DenseMatrix((1.0, 0.0), (1.0, 1.0)), DenseVector(10, 1))
    val nd6 = Parallelotope(DenseVector(Double.NegativeInfinity, -11), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity, -9))
    assertResult(nd6) { nd5.nonDeterministicAssignment(0) }
    assert(empty.nonDeterministicAssignment(0).isEmpty)
  }

  test("linear inequalities") {
    val li1 = Parallelotope(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (1.0, -1.0)), DenseVector(0, 0))
    assertResult(li1) { diamond.linearInequality(LinearForm(1.0, 2, 0)) }
    assertResult(li1) { diamond.linearInequality(LinearForm(1.0, 2)) }
    assert(empty.linearInequality(LinearForm(-1.0, 1, 0)).isEmpty)
    val full1 = Parallelotope.top(1)
    val li2 = Parallelotope(DenseVector(-1), DenseMatrix(1.0), DenseVector(1))
    assertResult(li2) {full1.linearInequality(LinearForm(-1.0, 1)).linearInequality(LinearForm(-1.0, -1)) }
  }

  test("linear disequalities") {
    val li1 = Parallelotope(DenseVector(-1, 0), DenseMatrix((1.0, 1.0), (1.0, -2.0)), DenseVector(0, 0))
    assertResult (li1) { li1.linearDisequality(1.0)}
    assertResult (empty) { li1.linearDisequality(0.0)}
    assertResult (li1) { li1.linearDisequality(LinearForm(1.0, 0, 1))}
    assertResult (li1) { li1.linearDisequality(LinearForm(0.5, 1, -2))}
    assertResult (empty) { li1.linearDisequality(LinearForm(0.0, 1, -2))}
  }

  test("union") {
    val u1 = Parallelotope(DenseVector(2, 0), DenseMatrix.eye(2), DenseVector(4, 2))
    val u2 = Parallelotope(DenseVector(-4, -1), DenseMatrix((-1.0, 3.0), (0.0, 1.0)), DenseVector(4, 2))
    assertResult(u2) { box union u1 }
    val u3 = Parallelotope(DenseVector(-1, -1), DenseMatrix((0.0, 1.0), (1.0, -1.0)), DenseVector(2, 4))
    assertResult(u3) { u1 union diamond }
    val u4 = Parallelotope(DenseVector(-4, 0), DenseMatrix.eye(2), DenseVector(-2, 2))
    val u5 = Parallelotope(DenseVector(-4, 0), DenseMatrix.eye(2), DenseVector(4, 2))
    assertResult(u5) { u4 union u1 }
    val u6 = Parallelotope(DenseVector(1, Double.NegativeInfinity), DenseMatrix((1.0, 0.0), (1.0, -1.0)), DenseVector(1, 1))
    val u7 = Parallelotope(DenseVector(0, Double.NegativeInfinity), DenseMatrix((1.0, 0.0), (0.0, -1.0)), DenseVector(0, 0))
    val u8 = Parallelotope(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(1, Double.PositiveInfinity))
    assertResult(u8) { u6 union u7 }
    val u9 = Parallelotope(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(0, Double.PositiveInfinity))
    assertResult(u8) { u9 union u8 }
    assertResult(u8) { u8 union u9 }
    val u10 = Parallelotope(DenseVector(2, 0), DenseMatrix.eye(2), DenseVector(2, 0))
    val u11 = Parallelotope(DenseVector(0, 2), DenseMatrix((0.0, 1.0), (1.0, -2.0)), DenseVector(1, 6))
    assertResult(u11) { u10 union u11 }
    val u12 = Parallelotope(DenseVector(1.0,0.0,0.0), DenseMatrix((1.0,0.0,0.0),(-1.0,1.0,0.0),(-1.0,0.0,1.0)), DenseVector(11.0,0.0,0.0)) 
    assertResult(box) { box union box }
    assertResult(diamond) { diamond union diamond }
    assertResult(u12) { u12 union u12 }
  }

  test("minimization, maximization and frequency") {
    val i = Parallelotope(DenseVector(-4, -1, 0), DenseMatrix((-1.0, 3.0, 0.0), (0.0, 1.0, 0.0),(-1.0, -1.0, 1.0)), DenseVector(4, 2, 0))
    assertResult(12)(i.maximize(LinearForm(0, 1, 1, 0)))
    assertResult(-8)(i.minimize(LinearForm(0, 1, 1, 0)))
    assertResult(None)(i.frequency(LinearForm(0, 1, 1, 0)))
    assertResult(Some(0))(i.frequency(LinearForm(0, -1, -1, 1)))
  }

  test("dimensional variation") {
    val i = diamond
    val j = Parallelotope(DenseVector(-1, -1, Double.NegativeInfinity), DenseMatrix((1.0, 1.0, 0.0),
        (1.0, -1.0, 0.0), (0.0, 0.0, 1.0)), DenseVector(1, 1, Double.PositiveInfinity))
    val h = Parallelotope(DenseVector(-1,  Double.NegativeInfinity), DenseMatrix((1.0, 0.0),
         (0.0, 1.0)), DenseVector(1, Double.PositiveInfinity))
    assertResult(j)(i.addVariable())
    assertResult(h)(j.delVariable(0))
    assertResult(h)(j.delVariable(1))
    assertResult(i)(j.delVariable(2))
  }

  test("dimensional maps") {
    val i = diamond
    val h = Parallelotope(DenseVector(-1), DenseMatrix((1.0)), DenseVector(1))
    assertResult(diamond) (diamond.mapVariables(Seq(1, 0)))
    assertResult(diamond)(i.mapVariables(Seq(0, 1)))
    assertResult(h)(i.mapVariables(Seq(-1, 0)))
    assertResult(diamond) (diamond.addVariable.mapVariables(Seq(1,0,-1)))
  }

  test("string representation") {
    assertResult("[ -1.0 <= x+y <= 1.0 , -1.0 <= x-y <= 1.0 ]") { diamond.mkString(Seq("x", "y")) }
    assertResult("empty") { empty.toString }
    assertResult("[ -Infinity <= v0 <= Infinity , -Infinity <= v1 <= Infinity ]") { full.toString }
  }

}
