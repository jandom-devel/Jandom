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

package it.unich.sci.jandom.domains.numerical.ppl

import org.scalatest.FunSuite
import parma_polyhedra_library.Octagonal_Shape_double
import parma_polyhedra_library.Double_Box
import it.unich.sci.jandom.domains.numerical.LinearForm

/**
 * Test suite for the PPLProperty numerical domain.
 * @author Gianluca Amato <g.amato@unich.it>
 */
class PPLPropertySuite extends FunSuite {
  val octDomain = PPLDomain[Octagonal_Shape_double]()

  val full = octDomain.top(3)
  val empty = octDomain.bottom(3)

  test("full should be full") {
    expectResult(true) { full.isTop }
  }

  test("full should not be empty") {
    expectResult(false) { full.isEmpty }
  }

  test("empty should be empty") {
    expectResult(true) { empty.isEmpty }
  }

  test("empty should not be full") {
    expectResult(false) { empty.isTop }
  }

  test("empty should be strictly less than full") {
    expectResult(true) { empty < full }
    expectResult(true) { empty <= full }
  }

  test("minimization/maximization") {
    val obj = full.
      linearAssignment(0, 0.0).
      linearInequality(LinearForm(-1, 0, 1, 1)).
      linearInequality(LinearForm(-1, 0, 1, -1))
    expectResult(Double.PositiveInfinity) { obj.maximize(LinearForm(0, 0, 0, 1)) }
    expectResult(1) { obj.maximize(LinearForm(0, 1, 1, 0)) }
    expectResult(None) { obj.frequency(LinearForm(0, 1, 0, 1)) }
    expectResult(Some(0)) { obj.frequency(LinearForm(0, 1, 0, 0)) }
  }

  test("various operations") {
    val obj = full.linearAssignment(0, 0.0)
    val obj2 = full.linearAssignment(1, 0.0)
    val obj3 = full.linearAssignment(2, 0.0)
    val obj4 = full.linearAssignment(2, 1.0)
    val obj5 = obj4 union obj3
    expectResult(true) { obj5 > obj4 }
    val obj7 = obj5.linearInequality(LinearForm(1, 0, 0, 1))
    expectResult(empty) { obj7 }
    val obj8 = obj4 widening obj3
    expectResult(obj5) { obj8 }
  }

  test("disequality do not crash") {
    val obj = full.linearAssignment(0, 0.0)
    val dis = obj.linearDisequality(LinearForm(0, 1, 0, 0))
    assert(true)
  }

  test("disequality is precise on boxes") {
    val boxDomain = PPLDomain[Double_Box]()
    val obj = boxDomain.top(3).linearAssignment(0, 0.0)
    expectResult(boxDomain.bottom(3)) { obj.linearDisequality(LinearForm(0, 1, 0, 0)) }
  }

  test("string conversion") {
    val obj = full.linearInequality(LinearForm(1, 1, 1, 0))
    val obj2 = obj.linearInequality(LinearForm(2, 1, 0, 0))
    expectResult("[ -x >= 2 , -x - y >= 1 ]") { obj2.mkString(Seq("x", "y", "z")) }
    expectResult("[ -v0 >= 2 , -v0 - v1 >= 1 ]") { obj2.toString }
  }

  test("string conversion for high-dimensional spaces") {
    val a = Array.fill(34)(0.0)
    a(27) = 1.0
    val obj3 = octDomain.top(33).linearInequality(LinearForm.v(27))
    expectResult("[ -v27 >= 0 ]") { obj3.toString }
  }

  test("map dimensions") {
    val obj = full.linearInequality(LinearForm(1, 1, 1, 0)).linearInequality(LinearForm(2, 1, 0, 0))
    val obj2 = full.linearInequality(LinearForm(1, 1, 1, 0)).linearInequality(LinearForm(2, 0, 1, 0))
    val obj3 = octDomain.top(2).linearInequality(LinearForm(2, 1, 0))

    expectResult(obj)(obj.mapVariables(Seq(0, 1, 2)))
    expectResult(obj2)(obj.mapVariables(Seq(1, 0, 2)))
    expectResult(obj3)(obj2.mapVariables(Seq(-1, 0, 1)))
  }

  test("multiplication") {
    val obj = full.linearInequality(LinearForm(1, 1, 1, 0)).linearInequality(LinearForm(2, 0, 1, 1)).linearAssignment(0, LinearForm(2, 0, 0, 0))
    expectResult( obj.linearAssignment(0, LinearForm(0, 0, 0, 2)) ) {  obj.variableMul(0, 2) }
    expectResult( obj.linearAssignment(2, LinearForm(0, 0, 0, 2)) ) {  obj.variableMul(2, 0) }
    assert ( obj.nonDeterministicAssignment(1) >= obj.variableMul(1,2) )
  }

  test("connect") {
    val obj1 = full.
      linearAssignment(0, LinearForm(3, 0, 0, 1)).
      linearInequality(LinearForm(-10, 0, 0, 1))
    val obj2 = full.
      linearAssignment(0, 1.0).
      linearAssignment(1, 3.0).
      linearInequality(LinearForm(0, 0, 1, 1))
    val obj3 = octDomain.top(4).
      linearAssignment(0, LinearForm(4, 0, 0, 0, 0)).
      linearAssignment(2, LinearForm(3, 0, 0, 0, 0)).
      linearInequality(LinearForm(0, 0, 0, 1, 1))
    expectResult(obj3)(obj1.connect(obj2, 1))
  }
}
