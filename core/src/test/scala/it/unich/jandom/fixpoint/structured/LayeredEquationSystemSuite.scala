/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.fixpoint.structured

import org.scalatest.FunSpec
import it.unich.jandom.fixpoint._
import it.unich.jandom.fixpoint.finite.DFOrdering
import it.unich.jandom.utils.Relation
import it.unich.jandom.fixpoint.finite.FiniteEquationSystem

class LayeredEquationSystemSuite extends FunSpec {

  val edges = Set('a', 'b', 'c', 'd', '*')
  val unknowns = Set(0, 1, 2, 3)
  val simpleEqs = LayeredEquationSystem[Int, Int, Char](
    edgeBody = { (e: Char) =>
      { (rho: Int => Int) =>
        {
          (x: Int) =>
            e match {
              case '*' => 1
              case 'a' => rho(0)
              case 'b' => rho(1) min 10
              case 'c' => rho(2) + 1
              case 'd' => rho(3)
            }
        }
      }
    },
    sources = Relation(Set(('a', 0), ('b', 1), ('c', 2), ('d', 3)), edges, unknowns),
    targets = Relation(Set(('*', 0), ('a', 1), ('b', 2), ('c', 3), ('d', 1)), edges, unknowns),
    unknowns = unknowns,
    inputUnknowns = Set(0),
    initial = { _ => 0 },
    combine = { _ max _ })
  val rho = { x: Int => x }

  describe("A Simple Layered equation system") {
    it("correctly computes the body") {
      val body = simpleEqs.body
      assertResult(1)(body(rho)(0))
      assertResult(3)(body(rho)(1))
      assertResult(1)(body(rho)(2))
      assertResult(3)(body(rho)(3))
    }

    it("correctly computes the body with dependencies") {
      val body = simpleEqs.bodyWithDependencies
      assertResult(1 -> Set())(body(rho)(0))
      assertResult(3 -> Set(0,3))(body(rho)(1))  // problem with ordering
      assertResult(1 -> Set(1))(body(rho)(2))
      assertResult(3 -> Set(2))(body(rho)(3))
    }

    it("correctly computes the influence") {
      val infl = simpleEqs.infl
      assertResult(Set(1))(infl.image(0))
      assertResult(Set(2))(infl.image(1))
      assertResult(Set(3))(infl.image(2))
      assertResult(Set(1))(infl.image(3))
    }

    it("correctly adds input assignments") {
      val input: PartialFunction[Int, Int] = { case _ => 2 }
      val eqs = simpleEqs.withInputAssignment(input)
      val body = eqs.body
      assertResult(2)(body(rho)(0))
      assertResult(3)(body(rho)(1))
      assertResult(2)(body(rho)(2))
      assertResult(3)(body(rho)(3))
    }

    it("correctly adds boxes") {
      def test(eqs: FiniteEquationSystem[Int, Int]) = {
        val body = eqs.body
        assertResult(2)(body(rho)(0))
        assertResult(7)(body(rho)(1))
        assertResult(4)(body(rho)(2))
        assertResult(9)(body(rho)(3))
      }

      val box = { (x: Int, y: Int) => x + 2 * y }
      val boxes: PartialFunction[Int, Box[Int]] = { case _ => box }
      val eqs1 = simpleEqs.withBoxes(boxes, true)
      val eqs2 = simpleEqs.withBoxes(boxes, false)

      test(eqs1)
      test(eqs2)
      for (x <- unknowns) {
        assert(simpleEqs.infl.image(x) === eqs1.infl.image(x))
        assert((simpleEqs.infl.image(x) union Set(x)) === eqs2.infl.image(x))
      }
    }

    it("correctly adds localized idempotent boxes") {
      def test(eqs: FiniteEquationSystem[Int, Int]) = {
        val body = eqs.body
        val rho2 = { x: Int => if (x == 0) 9 else x }
        assertResult(1)(body(rho)(0))
        assertResult(7)(body(rho)(1))
        assertResult(1)(body(rho)(2))
        assertResult(3)(body(rho)(3))
        assertResult(9)(body(rho2)(1))
      }

      val box = { (x: Int, y: Int) => x + (2 * y) }
      val boxes: PartialFunction[Int, Box[Int]] = { case _ => box }
      val ordering = DFOrdering(simpleEqs.infl)(0)
      val eqs1 = simpleEqs.withLocalizedBoxes(boxes, ordering, true)
      val eqs2 = simpleEqs.withLocalizedBoxes(boxes, ordering, false)

      test(eqs1)
      test(eqs2)
      for (x <- unknowns) {
        assert(simpleEqs.infl.image(x) === eqs1.infl.image(x))
        if (x != 1)
          assert(simpleEqs.infl.image(x) === eqs2.infl.image(x))
        else
          assert((simpleEqs.infl.image(x) union Set(x)) === eqs2.infl.image(x))
      }
    }
  }
}
