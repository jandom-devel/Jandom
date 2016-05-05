/**
 * Copyright 2013, 2016 Gianluca Amato
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

package it.unich.jandom.domains.numerical

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import it.unich.jandom.domains.EmptyExistsSuite
import it.unich.jandom.domains.SeparatedTopAndBottomSuite

/**
 * Test suite for the parallelotope domain.
 * @author Gianluca Amato <gamato@unich.it>
 */
class ParallelotopeDomainSuite extends NumericalDomainSuite with SeparatedTopAndBottomSuite with EmptyExistsSuite {
  lazy val dom = ParallelotopeDomain()

  val box = dom(DenseVector(-1, -1), DenseMatrix.eye(2), DenseVector(1, 1))
  val diamond = dom(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (1.0, -1.0)), DenseVector(1, 1))
  val empty = dom.bottom(2)
  val full = dom.top(2)

  describe("constructors") {
    they("should only work with compatible sizes of bounds and shapes") {
      intercept[IllegalArgumentException] { dom(DenseVector(0, 2), DenseMatrix.eye(2), DenseVector(0, 2, 3)) }
    }
  }

  describe("constructors and extractors for non-trivial parallelotopes") {
    assertResult(2) { box.dimension }
    assertResult(false) { box.isEmpty }
    assertResult(false) { box.isTop }
  }

  describe("constructors and extractors for full parallelotopes") {
    assertResult(2) { full.dimension }
    assertResult(false) { full.isEmpty }
    assertResult(true) { full.isTop }
  }

  describe("constructors and extractors for empty parallelotopes") {
    assertResult(2) { empty.dimension }
    assertResult(true) { empty.isEmpty }
    assertResult(false) { empty.isTop }
  }

  describe("comparison of parallelotopes") {
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
    val box2 = dom(DenseVector(-0.5, -0.5), DenseMatrix.eye(2), DenseVector(0.5, 0.5))
    assert(box2 <= box)
    assert(box >= box2)
    assert(box2 < box)
    assert(box > box2)
    val box3 = dom(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(2, 2))
    assertResult(None) { box.tryCompareTo(box3) }
  }

  describe("rotation of shapes") {
    val m = DenseMatrix((1.0, 1.0), (-1.0, 1.0))
    val protcalc = box.rotate(m)
    val protdef = dom(DenseVector(-2, -2), m, DenseVector(2, 2))
    assertResult(protdef) { protcalc }
  }

  describe("linear invertible assignment") {
    val li1 = dom(DenseVector(0, -1), DenseMatrix((1.0, -1.0), (0.0, 1.0)), DenseVector(2, 1))
    assertResult(li1) { box.linearAssignment(0, LinearForm(1, 1, 1)) }
    val li2 = dom(DenseVector(1, -1), DenseMatrix((1.0, 0.0), (-1.0, 1.0)), DenseVector(1, 0))
    val li3 = dom(DenseVector(2, -2), DenseMatrix((1.0, 0.0), (-1.0, 1.0)), DenseVector(2, -1))
    assertResult(li3) { li2.linearAssignment(0, LinearForm(1, 1, 0)) }
    assertResult(li3) { li2.linearAssignment(0, LinearForm(1, 1)) }
    val li4 = dom(DenseVector(-1, -2), DenseMatrix((1.0, 0.0), (-1.0, 1.0)), DenseVector(1, 2))
    assertResult(li4) { box.linearAssignment(1, LinearForm(0, 1, 2)) }
    assert(empty.linearAssignment(1, LinearForm(0.0, 1, 1)).isEmpty)
  }

  describe("non-invertible linear assignment") {
    val ln1 = dom(DenseVector(2, -1), DenseMatrix((1.0, -1.0), (0.0, 1.0)), DenseVector(2, 1))
    assertResult(ln1) { box.linearAssignment(0, LinearForm(2, 0, 1)) }
    val ln2 = dom(DenseVector(0, Double.NegativeInfinity), DenseMatrix((-1.0, 1.0), (0.0, 1.0)), DenseVector(0, Double.PositiveInfinity))
    val ln3 = dom(DenseVector(Double.NegativeInfinity, 0), DenseMatrix((1.0, -1.0), (0.0, 1.0)), DenseVector(Double.PositiveInfinity, 0))
    assertResult(ln2) { ln3.linearAssignment(1, LinearForm(0, 1, 0)) }
    assertResult(ln2) { ln3.linearAssignment(1, LinearForm(0, 1)) }
    assert(empty.linearAssignment(1, LinearForm(0, 1, 0)).isEmpty)
  }

  describe("non-deterministic assignment") {
    val nd1 = dom(DenseVector(Double.NegativeInfinity, -1), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity, 1))
    assertResult(nd1) { box.nonDeterministicAssignment(0) }
    assertResult(nd1) { nd1.nonDeterministicAssignment(0) }
    assertResult(nd1) { diamond.nonDeterministicAssignment(0) }
    val nd2 = dom(DenseVector(0, 0), DenseMatrix((2.0, 1.0), (2.0, -1.0)), DenseVector(1, 1))
    val nd3 = dom(DenseVector(Double.NegativeInfinity, -1), DenseMatrix((2.0, 1.0), (0.0, -2.0)), DenseVector(Double.PositiveInfinity, 1))
    assertResult(nd3) { nd2.nonDeterministicAssignment(0) }
    val nd4 = dom(DenseVector(Double.NegativeInfinity, 0), DenseMatrix((2.0, 1.0), (4.0, 0.0)), DenseVector(Double.PositiveInfinity, 2))
    assertResult(nd4) { nd2.nonDeterministicAssignment(1) }
    val nd5 = dom(DenseVector(10, -1), DenseMatrix((1.0, 0.0), (1.0, 1.0)), DenseVector(10, 1))
    val nd6 = dom(DenseVector(Double.NegativeInfinity, -11), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity, -9))
    assertResult(nd6) { nd5.nonDeterministicAssignment(0) }
    assert(empty.nonDeterministicAssignment(0).isEmpty)
  }

  describe("linear inequalities") {
    val li1 = dom(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (1.0, -1.0)), DenseVector(0, 0))
    assertResult(li1) { diamond.linearInequality(LinearForm(1, 2, 0)) }
    assertResult(li1) { diamond.linearInequality(LinearForm(1, 2)) }
    assert(empty.linearInequality(LinearForm(-1, 1, 0)).isEmpty)
  }

  describe("linear disequalities") {
    val li1 = dom(DenseVector(-1, 0), DenseMatrix((1.0, 1.0), (1.0, -2.0)), DenseVector(0, 0))
    assertResult(li1) { li1.linearDisequality(1.0) }
    assertResult(empty) { li1.linearDisequality(0.0) }
    assertResult(li1) { li1.linearDisequality(LinearForm(1, 0, 1)) }
    assertResult(li1) { li1.linearDisequality(LinearForm(0.5, 1, -2)) }
    assertResult(empty) { li1.linearDisequality(LinearForm(0, 1, -2)) }
  }

  describe("union") {
    val u1 = dom(DenseVector(2, 0), DenseMatrix.eye(2), DenseVector(4, 2))
    val u2 = dom(DenseVector(-4, -1), DenseMatrix((-1.0, 3.0), (0.0, 1.0)), DenseVector(4, 2))
    assertResult(u2) { box union u1 }
    val u3 = dom(DenseVector(-1, -1), DenseMatrix((0.0, 1.0), (1.0, -1.0)), DenseVector(2, 4))
    assertResult(u3) { u1 union diamond }
    val u4 = dom(DenseVector(-4, 0), DenseMatrix.eye(2), DenseVector(-2, 2))
    val u5 = dom(DenseVector(-4, 0), DenseMatrix.eye(2), DenseVector(4, 2))
    assertResult(u5) { u4 union u1 }
    val u6 = dom(DenseVector(1, Double.NegativeInfinity), DenseMatrix((1.0, 0.0), (1.0, -1.0)), DenseVector(1, 1))
    val u7 = dom(DenseVector(0, Double.NegativeInfinity), DenseMatrix((1.0, 0.0), (0.0, -1.0)), DenseVector(0, 0))
    val u8 = dom(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(1, Double.PositiveInfinity))
    assertResult(u8) { u6 union u7 }
    val u9 = dom(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(0, Double.PositiveInfinity))
    assertResult(u8) { u9 union u8 }
    assertResult(u8) { u8 union u9 }
    val u10 = dom(DenseVector(2, 0), DenseMatrix.eye(2), DenseVector(2, 0))
    val u11 = dom(DenseVector(0, 2), DenseMatrix((0.0, 1.0), (1.0, -2.0)), DenseVector(1, 6))
    assertResult(u11) { u10 union u11 }
  }

  describe("minimization, maximization and frequency") {
    val i = dom(DenseVector(-4, -1, 0), DenseMatrix((-1.0, 3.0, 0.0), (0.0, 1.0, 0.0), (-1.0, -1.0, 1.0)), DenseVector(4, 2, 0))
    assertResult(12)(i.maximize(LinearForm(0, 1, 1, 0)))
    assertResult(-8)(i.minimize(LinearForm(0, 1, 1, 0)))
    assertResult(None)(i.frequency(LinearForm(0, 1, 1, 0)))
    assertResult(Some(0))(i.frequency(LinearForm(0, -1, -1, 1)))
  }

  describe("dimensional variation") {
    val i = diamond
    val j = dom(DenseVector(-1, -1, Double.NegativeInfinity), DenseMatrix((1.0, 1.0, 0.0),
      (1.0, -1.0, 0.0), (0.0, 0.0, 1.0)), DenseVector(1, 1, Double.PositiveInfinity))
    val h = dom(DenseVector(-1, Double.NegativeInfinity), DenseMatrix((1.0, 0.0),
      (0.0, 1.0)), DenseVector(1, Double.PositiveInfinity))
    assertResult(j)(i.addVariable())
    assertResult(h)(j.delVariable(0))
    assertResult(h)(j.delVariable(1))
    assertResult(i)(j.delVariable(2))
  }

  describe("dimensional maps") {
    val i = diamond
    val h = dom(DenseVector(-1), DenseMatrix((1.0)), DenseVector(1))
    assertResult(diamond)(diamond.mapVariables(Seq(1, 0)))
    assertResult(diamond)(i.mapVariables(Seq(0, 1)))
    assertResult(h)(i.mapVariables(Seq(-1, 0)))
    assertResult(diamond)(diamond.addVariable.mapVariables(Seq(1, 0, -1)))
  }

  describe("string representation") {
    assertResult("[ -1.0 <= x+y <= 1.0 , -1.0 <= x-y <= 1.0 ]") { diamond.mkString(Seq("x", "y")) }
    assertResult("empty") { empty.toString }
    assertResult("[ -Infinity <= v0 <= Infinity , -Infinity <= v1 <= Infinity ]") { full.toString }
  }

  describe("all parallelotopes are polyhedral") {
     forAll(someProperties) { (p) => assert(p.isPolyhedral) }
  }

  // I am not sure it works for all possible cases due to rounding errors.
  describe("all parallelotopes may be rebuilt from constraints") {
    forAll(someProperties) { (p) =>
      assertResult(p) { p.constraints.foldLeft(p.top) { (prop, lf) => prop.linearInequality(lf) } }
    }
  }

}
