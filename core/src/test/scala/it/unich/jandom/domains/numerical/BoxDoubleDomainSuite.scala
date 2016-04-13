/**
 * Copyright 2013, 2014 Gianluca Amato
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

package it.unich.jandom.domains.numerical

import it.unich.jandom.domains.{EmptyExistsSuite, SeparatedTopAndBottomSuite}
import it.unich.jandom.domains.PreciseIntersectionSuite

/**
 * A private trait for all BoxDoubleDomain suites.
 * @author Gianluca Amato <gamato@unich.it>
 */
private[numerical] trait BoxDoubleDomainSuite extends NumericalDomainSuite with SeparatedTopAndBottomSuite with EmptyExistsSuite with PreciseIntersectionSuite {

  val dom: BoxDoubleDomain

  override lazy val someProperties = Table("property", dom(Array(1, 2), Array(5, 4)), dom(Array(0, 3), Array(3, 4)), dom(Array(0, 2), Array(3, 6)),
    dom(Array(0, 0), Array(5, 5)), dom.top(2), dom.bottom(2))

  override lazy val someLinearForms = Table[LinearForm[Double]]("linear form", LinearForm(1, 1, 1), LinearForm(0), LinearForm(0, -1, 0),
    LinearForm(2, 1, 1), LinearForm(2, 1, 0))

  describe("constructors") {
    they("should return an exception for non-normalized bounds") {
      intercept[IllegalArgumentException] { dom(Array(0, 2), Array(0, 2, 3)) }
      intercept[IllegalArgumentException] { dom(Array(Double.PositiveInfinity, 2), Array(0, 2, 3)) }
      intercept[IllegalArgumentException] { new dom.Property(Array(0, 2), Array(0, 2, 3), true) }
    }
  }

  describe("Test on union and intersection") {
    val i = dom(Array(1, 2), Array(5, 4))
    val j = dom(Array(0, 3), Array(3, 4))
    assertResult(dom(Array(0, 2), Array(5, 4))) { i union j }
    assertResult(dom(Array(1, 3), Array(3, 4))) { i intersection j }
  }

  describe("Test on widening and narrowing") {
    val i = dom(Array(1, 2), Array(5, 4))
    val j = dom(Array(0, 2), Array(3, 6))
    val w = i widening j
    assertResult(i)(dom.bottom(2) widening i)
    assertResult(dom(Array(Double.NegativeInfinity, 2), Array(5, Double.PositiveInfinity)))(w)
    assertResult(w narrowing j)(i union j)
  }

  describe("Test on empty boxes") {
    val i = dom(Array(-1, -2), Array(-4, 3))
    val j = dom(Array(0, 0), Array(5, 5))
    assertResult(dom.bottom(2)) { i }
    assertResult(j) { i union j }
    assertResult(i) { i intersection j }
    assertResult(i) { i.linearAssignment(1, LinearForm(1, 1, 1)) }
    assertResult(i) { i.linearAssignment(1, LinearForm(0)) }
    intercept[IllegalArgumentException] { i.linearAssignment(-1, LinearForm(1, 1, 1)) }
    intercept[IllegalArgumentException] { i.linearAssignment(2, LinearForm(1, 1, 1)) }
  }

  describe("Test on linear inequations") {
    val i = dom.top(2).linearInequality(LinearForm(-3, 1, 0))
    val j = dom(Array(0, 0), Array(5, 5)).linearInequality(LinearForm(-4, 1, 1))
    assert(dom(Array(Double.NegativeInfinity, Double.NegativeInfinity), Array(3, Double.PositiveInfinity)) == i)
    assert(dom(Array(0, 0), Array(4, 4)) <= j)
  }

  describe("Test on linear inequalities and disequalities") {
    val i = dom.top(2).linearInequality(LinearForm.v(0))
    assertResult(i) { i.linearDisequality(LinearForm.v(0)) }
    val j = i.linearInequality(LinearForm(0, -1, 0))
    assert(dom.bottom(2) <= j.linearDisequality(LinearForm.v(0)))
    assertResult(dom.bottom(2)) { j.linearDisequality(LinearForm(0)) }
    assertResult(j) { j.linearDisequality(LinearForm(2)) }
    assertResult(j) { j.linearDisequality(LinearForm(2, 1, 1)) }
    assertResult(j) { j.linearDisequality(LinearForm(2, 1, 0)) }
  }

  describe("Test on non deterministic assignment") {
    val i = dom(Array(0, 0), Array(5, 5))
    val j = dom(Array(0, Double.NegativeInfinity), Array(5, Double.PositiveInfinity))
    val l = dom(Array(Double.NegativeInfinity, 0), Array(Double.PositiveInfinity, 5))
    assertResult(j) { i.nonDeterministicAssignment(1) }
    assertResult(l) { i.nonDeterministicAssignment(0) }
    intercept[IllegalArgumentException] { i.nonDeterministicAssignment(-1) }
    intercept[IllegalArgumentException] { i.nonDeterministicAssignment(2) }
  }

  describe("Test on change of fibers") {
    val i = dom(Array(0, 0), Array(1, 2))
    val j = dom(Array(0, 0, Double.NegativeInfinity), Array(1, 2, Double.PositiveInfinity))
    val h = dom(Array(0, Double.NegativeInfinity), Array(1, Double.PositiveInfinity))
    assertResult(j)(i.addVariable())
    assertResult(h)(j.delVariable(1))
    assertResult(i)(j.delVariable(2))
    intercept[IllegalArgumentException] { i.delVariable(-1) }
    intercept[IllegalArgumentException] { i.delVariable(2) }
  }

  describe("Test on map of dimensions") {
    val i = dom(Array(0, 0), Array(1, 2))
    val j = dom(Array(0, 0), Array(2, 1))
    val h = dom(Array(0), Array(2))
    assertResult(j)(i.mapVariables(Seq(1, 0)))
    assertResult(i)(i.mapVariables(Seq(0, 1)))
    assertResult(h)(i.mapVariables(Seq(-1, 0)))
  }

  describe("Test on minimization, maximization and frequency") {
    val i = dom(Array(0, 0), Array(1, 2))
    assert(i.maximize(LinearForm(0, 1, 1)) >= 3)
    assert(i.minimize(LinearForm(0, 1, 1)) <= 0)
    assertResult(None)(i.frequency(LinearForm(0, 1, 1)))
    val j = dom(Array(0, 1, 0), Array(0, 1, 1))
    assertResult(None)(j.frequency(LinearForm(0, 1, 1, 1)))
    assertResult(None)(j.frequency(LinearForm(0, 0, 0, 1)))

  }

  describe("Test on string conversion") {
    val i = dom(Array(0, -1), Array(2, 3))
    assertResult("[ 0.0 <= x <= 2.0 , -1.0 <= y <= 3.0 ]") { i.mkString(Seq("x", "y")) }
    assertResult("[ 0.0 <= v0 <= 2.0 , -1.0 <= v1 <= 3.0 ]") { i.toString }
  }
}

/**
 * This is a unit test for the BoxDouble domain over doubles. These properties
 * do not work for reals due to the artificial enlargements which happens in
 * that domain.
 * @author Gianluca Amato <gamato@unich.it>
 */
class BoxDoubleDomainDoubleSuite extends BoxDoubleDomainSuite {
  lazy val dom = BoxDoubleDomain(overReals = false)

  describe("Precise thest on minimization, maximization and frequency") {
    val i = dom(Array(0, 0), Array(1, 2))
    assertResult(3)(i.maximize(LinearForm(0, 1, 1)))
    assertResult(0)(i.minimize(LinearForm(0, 1, 1)))
    val j = dom(Array(0, 1, 0), Array(0, 1, 1))
    assertResult(Some(0))(j.frequency(LinearForm(0, 1, 0, 0)))
    assertResult(Some(1))(j.frequency(LinearForm(0, 0, 1, 0)))
  }

  describe("Precise test on inequalities and disequalities") {
    val i = dom.top(2).linearInequality(LinearForm.v(0))
    val j = i.linearInequality(LinearForm(0, -1, 0))
    assert(j.linearDisequality(LinearForm.v(0)).isBottom)
  }

  describe("Precise test on linear inequations") {
    val j = dom(Array(0, 0), Array(5, 5)).linearInequality(LinearForm(-4, 1, 1))
    assert(dom(Array(0, 0), Array(4, 4)) == j)
  }

  describe("All boxes are polyhedral") {
    forAll(someProperties) { (p) => p.isPolyhedral }
  }

  // I am not sure it works for all possible cases due to rounding errors.
  describe("All boxes may be rebuilt from constraints") {
    forAll(someProperties) { (p) =>
      assertResult(p) { p.constraints.foldLeft(p.top) { (prop, lf) => prop.linearInequality(lf) } }
    }
  }
}

/**
 * This is a unit test for the BoxDouble domain over reals.
 * @author Gianluca Amato <gamato@unich.it>
 */
class BoxDoubleDomainRealSuite extends BoxDoubleDomainSuite {
  lazy val dom = BoxDoubleDomain(overReals = true)
}
