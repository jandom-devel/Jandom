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

package it.unich.jandom.domains.numerical.ppl

import org.scalatest.FunSpec
import org.scalatest.prop.Tables.Table

import it.unich.jandom.domains.{ DomainTransformation, EmptyExistsSuite, SeparatedTopAndBottomSuite }
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.LinearForm.c
import it.unich.jandom.domains.numerical.NumericalDomainSuite

import parma_polyhedra_library.{ Double_Box, Octagonal_Shape_double }
import it.unich.jandom.domains.PreciseIntersectionSuite

/**
 * These are configuration parameters for all the tests relative the PPL-based
 * domains.
 * @author Gianluca Amato <gamato@unich.it>
 */
private[ppl] trait PPLDomainSuiteParameters[Shape <: AnyRef] {
  val dom: PPLDomain[Shape]
}

/**
 * Test suite for the PPLProperty numerical domain, instantiated with the Double_Box shape
 * @author Gianluca Amato <gamato@unich.it>
 */
class PPLDomainSuiteBox extends { val dom = PPLDomain[Double_Box]() } with FunSpec
  with PPLDomainSuiteParameters[Double_Box] with NumericalDomainSuite with SeparatedTopAndBottomSuite with EmptyExistsSuite
  with PreciseIntersectionSuite {

  describe("Test on disequality") {
    val obj = dom.top(3).linearAssignment(0, 0.0)
    assertResult(dom.bottom(3)) { obj.linearDisequality(LinearForm(0, 1, 0, 0)) }
  }

  describe("All PPLBoxes are polyhedral") {
    forAll(someProperties) { (p) => assert(p.isPolyhedral) }
  }

  describe("All PPLBoxes may be rebuilt from constraints") {
    forAll(someProperties) { (p) =>
      assertResult(p) { p.constraints.foldLeft(p.top) { (prop, lf) => prop.linearInequality(lf) } }
    }
  }
}

/**
 * Test suite for the PPLProperty numerical domain, instantiated with the Double_Box shape
 * @author Gianluca Amato <gamato@unich.it>
 */
class PPLDomainSuiteOctagon extends { val dom = PPLDomain[Octagonal_Shape_double]() } with FunSpec
  with PPLDomainSuiteParameters[Octagonal_Shape_double] with NumericalDomainSuite with SeparatedTopAndBottomSuite with EmptyExistsSuite {

  val full = dom.top(3)
  val empty = dom.bottom(3)

  describe("Test for minimization/maximization") {
    val obj = full.
      linearAssignment(0, 0.0).
      linearInequality(LinearForm(-1, 0, 1, 1)).
      linearInequality(LinearForm(-1, 0, 1, -1))
    assertResult(Double.PositiveInfinity) { obj.maximize(LinearForm(0, 0, 0, 1)) }
    assertResult(1) { obj.maximize(LinearForm(0, 1, 1, 0)) }
    assertResult(None) { obj.frequency(LinearForm(0, 1, 0, 1)) }
    assertResult(Some(0)) { obj.frequency(LinearForm(0, 1, 0, 0)) }
  }

  describe("Test for various operations") {
    val obj = full.linearAssignment(0, 0.0)
    val obj2 = full.linearAssignment(1, 0.0)
    val obj3 = full.linearAssignment(2, 0.0)
    val obj4 = full.linearAssignment(2, 1.0)
    val obj5 = obj4 union obj3
    assertResult(true) { obj5 > obj4 }
    val obj7 = obj5.linearInequality(LinearForm(1, 0, 0, 1))
    assertResult(empty) { obj7 }
    val obj8 = obj4 widening obj3
    assertResult(obj5) { obj8 }
  }

  describe("Test constraints / isPolyhedral methods") {
    val lf1 = LinearForm[Double](-1, 0, 1, 1)
    val lf2 = LinearForm[Double](-1, 0, 1, -1)
    assertResult(Seq()) { full.constraints }
    assertResult(true) { full.isPolyhedral }
    val obj = full.
      linearInequality(lf1).
      linearInequality(lf2)
    assertResult(Set(lf1, lf2)) { obj.constraints.toSet }
  }

  describe("Test that disequality do not crash") {
    val obj = full.linearAssignment(0, 0.0)
    val dis = obj.linearDisequality(LinearForm(0, 1, 0, 0))
  }

  describe("Test string conversion") {
    val obj = full.linearInequality(LinearForm(1, 1, 1, 0))
    val obj2 = obj.linearInequality(LinearForm(2, 1, 0, 0))
    assertResult("[ -x >= 2 , -x - y >= 1 ]") { obj2.mkString(Seq("x", "y", "z")) }
    assertResult("[ -v0 >= 2 , -v0 - v1 >= 1 ]") { obj2.toString }
  }

  describe("Test string conversion for high-dimensional spaces") {
    val a = Array.fill(34)(0.0)
    a(27) = 1.0
    val obj3 = dom.top(33).linearInequality(LinearForm.v(27))
    assertResult("[ -v27 >= 0 ]") { obj3.toString }
  }

  describe("Test map dimensions") {
    val obj = full.linearInequality(LinearForm(1, 1, 1, 0)).linearInequality(LinearForm(2, 1, 0, 0))
    val obj2 = full.linearInequality(LinearForm(1, 1, 1, 0)).linearInequality(LinearForm(2, 0, 1, 0))
    val obj3 = dom.top(2).linearInequality(LinearForm(2, 1, 0))

    assertResult(obj)(obj.mapVariables(Seq(0, 1, 2)))
    assertResult(obj2)(obj.mapVariables(Seq(1, 0, 2)))
    assertResult(obj3)(obj2.mapVariables(Seq(-1, 0, 1)))
  }

  describe("Test multiplication") {
    val obj = full.linearInequality(LinearForm(1, 1, 1, 0)).linearInequality(LinearForm(2, 0, 1, 1)).linearAssignment(0, LinearForm(2, 0, 0, 0))
    assertResult(obj.linearAssignment(0, LinearForm(0, 0, 0, 2))) { obj.variableMul(0, 2) }
    assertResult(obj.linearAssignment(2, LinearForm(0, 0, 0, 2))) { obj.variableMul(2, 0) }
    assert(obj.nonDeterministicAssignment(1) >= obj.variableMul(1, 2))
  }

  describe("Test connect") {
    val obj1 = full.
      linearAssignment(0, LinearForm(3, 0, 0, 1)).
      linearInequality(LinearForm(-10, 0, 0, 1))
    val obj2 = full.
      linearAssignment(0, 1.0).
      linearAssignment(1, 3.0).
      linearInequality(LinearForm(0, 0, 1, 1))
    val obj3 = dom.top(4).
      linearAssignment(0, LinearForm(4, 0, 0, 0, 0)).
      linearAssignment(2, LinearForm(3, 0, 0, 0, 0)).
      linearInequality(LinearForm(0, 0, 0, 1, 1))
    assertResult(obj3)(obj1.connect(obj2, 1))
  }

  describe("Test non integer coefficients") {
    val obj1 = full.linearInequality(LinearForm(0.5, 1, 1, 0))
    val obj2 = obj1.linearAssignment(2, LinearForm(0.25, 1, 0, 0))
    val m = obj2.maximize(LinearForm(0, 1, 0, -1))
    assertResult(-0.25) { m }
  }

  describe("Test constructor from other domains and transformers") {
    val octDomain = PPLDomain[Octagonal_Shape_double]()
    val boxDomain = PPLDomain[Double_Box]()

    val transform = implicitly[DomainTransformation[PPLDomain[_ <: AnyRef], PPLDomain[_ <: AnyRef]]]
    val diamond = octDomain.top(2).linearInequality(LinearForm(-1, 1, 1)).linearInequality(LinearForm(-1, -1, -1)).
      linearInequality(LinearForm(-1, 1, -1)).linearInequality(LinearForm(-1, -1, 1))
    val box = boxDomain.top(2).linearInequality(LinearForm(-1, 1, 0)).linearInequality(LinearForm(-1, -1, 0)).
      linearInequality(LinearForm(-1, 0, -1)).linearInequality(LinearForm(-1, 0, 1))
    assertResult(box) { boxDomain(diamond) }
    assertResult(box) { transform(octDomain, boxDomain)(diamond) }
  }

  describe("All PPLOctagons are polyhedral") {
    forAll(someProperties) { (p) => assert(p.isPolyhedral) }
  }

  describe("All PPLOctagons may be rebuilt from constraints") {
    forAll(someProperties) { (p) =>
      assertResult(p) { p.constraints.foldLeft(p.top) { (prop, lf) => prop.linearInequality(lf) } }
    }
  }
}
