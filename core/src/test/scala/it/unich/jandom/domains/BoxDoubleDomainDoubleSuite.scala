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

package it.unich.jandom.domains

import org.scalatest.FunSuite
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.BoxDoubleDomain

/**
 * This is a unit test for the BoxDouble numerical domain over doubles.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class BoxDoubleDomainDoubleSuite extends FunSuite {
  val BoxDouble = BoxDoubleDomain()

  test("constructors should only work with normalized bounds") {
    intercept[IllegalArgumentException] { BoxDouble(Array(0, 2), Array(0, 2, 3)) }
    intercept[IllegalArgumentException] { BoxDouble(Array(Double.PositiveInfinity, 2), Array(0, 2, 3)) }
  }

  test("lattice operation on boxes") {
    val i = BoxDouble(Array(1, 2), Array(5, 4))
    val j = BoxDouble(Array(0, 3), Array(3, 4))
    assertResult(BoxDouble(Array(0, 2), Array(5, 4))) { i union j }
    assertResult(BoxDouble(Array(1, 3), Array(3, 4))) { i intersection j }
  }

  test("widening and narrowing") {
	val i = BoxDouble(Array(1, 2), Array(5, 4))
    val j = BoxDouble(Array(0, 2), Array(3, 6))
    val w = i widening j
    assertResult(i)(BoxDouble.bottom(2) widening i)
    assertResult(BoxDouble(Array(Double.NegativeInfinity, 2), Array(5, Double.PositiveInfinity)))(w)
    assertResult(w narrowing j)(i union j)
  }

  test("empty boxes") {
    val i = BoxDouble(Array(-1, -2), Array(-4, 3))
    val j = BoxDouble(Array(0, 0), Array(5, 5))
    assertResult(BoxDouble.bottom(2)) { i }
    assertResult(j) { i union j }
    assertResult(i) { i intersection j }
    assertResult(i) { i.linearAssignment(1, LinearForm(1,1,1)) }
    assertResult(i) { i.linearAssignment(1, LinearForm(0)) }
    intercept[IllegalArgumentException] { i.linearAssignment(-1, LinearForm(1,1,1)) }
    intercept[IllegalArgumentException] { i.linearAssignment(2, LinearForm(1,1,1)) }
  }

  test("linear inequations") {
    val i = BoxDouble.top(2).linearInequality(LinearForm(-3, 1, 0))
    val j = BoxDouble(Array(0, 0), Array(5, 5)).linearInequality(LinearForm(-4, 1, 1))
    assertResult(BoxDouble(Array(Double.NegativeInfinity, Double.NegativeInfinity), Array(3, Double.PositiveInfinity))) { i }
    assertResult(BoxDouble(Array(0, 0), Array(4, 4))) { j }
  }

  test("inequalities and disequalities") {
    val i = BoxDouble.top(2).linearInequality(LinearForm.v(0))
    assertResult(i) { i.linearDisequality(LinearForm.v(0)) }
    val j = i.linearInequality(LinearForm(0,-1, 0))
    assertResult(BoxDouble.bottom(2)) { j.linearDisequality(LinearForm.v(0)) }
    assertResult(BoxDouble.bottom(2)) { j.linearDisequality(LinearForm(0)) }
    assertResult(j) { j.linearDisequality(LinearForm(2)) }
    assertResult(j) { j.linearDisequality(LinearForm(2, 1, 1)) }
    assertResult(j) { j.linearDisequality(LinearForm(2, 1, 0)) }
  }

  test("non deterministic assignment") {
    val i = BoxDouble(Array(0, 0), Array(5, 5))
    val j = BoxDouble(Array(0, Double.NegativeInfinity), Array(5, Double.PositiveInfinity))
    val l = BoxDouble(Array(Double.NegativeInfinity, 0), Array(Double.PositiveInfinity, 5))
    assertResult(j) { i.nonDeterministicAssignment(1) }
    assertResult(l) { i.nonDeterministicAssignment(0) }
    intercept[IllegalArgumentException] { i.nonDeterministicAssignment(-1) }
    intercept[IllegalArgumentException] { i.nonDeterministicAssignment(2) }
  }

  test("dimensional variation") {
    val i = BoxDouble(Array(0, 0), Array(1, 2))
    val j = BoxDouble(Array(0, 0, Double.NegativeInfinity), Array(1, 2, Double.PositiveInfinity))
    val h = BoxDouble(Array(0, Double.NegativeInfinity), Array(1, Double.PositiveInfinity))
    assertResult(j)(i.addVariable())
    assertResult(h)(j.delVariable(1))
    assertResult(i)(j.delVariable(2))
    intercept[IllegalArgumentException] { i.delVariable(-1) }
    intercept[IllegalArgumentException] { i.delVariable(2) }
  }

  test("dimensional maps") {
    val i = BoxDouble(Array(0, 0), Array(1, 2))
    val j = BoxDouble(Array(0, 0), Array(2, 1))
    val h = BoxDouble(Array(0), Array(2))
    assertResult(j)(i.mapVariables(Seq(1, 0)))
    assertResult(i)(i.mapVariables(Seq(0, 1)))
    assertResult(h)(i.mapVariables(Seq(-1, 0)))
  }

  test("minimization, maximization and frequency") {
    val i = BoxDouble(Array(0, 0), Array(1, 2))
    assertResult(3)(i.maximize(LinearForm(0, 1, 1)))
    assertResult(0)(i.minimize(LinearForm(0, 1, 1)))
    assertResult(None)(i.frequency(LinearForm(0, 1, 1)))
    val j = BoxDouble(Array(0, 1, 0), Array(0, 1, 1))
    assertResult(None)(j.frequency(LinearForm(0, 1, 1, 1)))
    assertResult(None)(j.frequency(LinearForm(0, 0, 0, 1)))
    assertResult(Some(0))(j.frequency(LinearForm(0, 1, 0, 0)))
    assertResult(Some(1))(j.frequency(LinearForm(0, 0, 1, 0)))
  }

  test("string conversion") {
    val i = BoxDouble(Array(0, -1), Array(2, 3))
    assertResult("[ 0.0 <= x <= 2.0 , -1.0 <= y <= 3.0 ]") { i.mkString(Seq("x", "y")) }
    assertResult("[ 0.0 <= v0 <= 2.0 , -1.0 <= v1 <= 3.0 ]") { i.toString }
  }
}
