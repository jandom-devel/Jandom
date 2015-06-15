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
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.fixpoint

import org.scalatest.path.FunSpec

class EquationSystemSuite extends FunSpec {
  val simpleEqs = EquationSystem[Int, Int](
    { (rho: Int => Int) =>
      x: Int =>
        x match {
          case 0 => rho(0)
          case 1 => (rho(0) max rho(2)) min rho(3)
          case 2 => rho(1) + 1
          case 3 => rho(3)
        }
    },
    initial = { x => x })

  val rho = simpleEqs.initial
  val box: Box[Int] = { _ * _ }

  describe("An equation system") {
    it("computes r.h.s. according to its body function") {
      assertResult(0) { simpleEqs.body(rho)(0) }
      assertResult(2) { simpleEqs.body(rho)(1) }
      assertResult(2) { simpleEqs.body(rho)(2) }
      assertResult(3) { simpleEqs.body(rho)(3) }
    }
    
    it("correctly infers dependencies") {
      assertResult((0, Seq(0))) { simpleEqs.bodyWithDependencies(rho)(0) }
      assertResult((2, Seq(0, 2, 3))) { simpleEqs.bodyWithDependencies(rho)(1) }
      assertResult((2, Seq(1))) { simpleEqs.bodyWithDependencies(rho)(2) }
      assertResult((3, Seq(3))) { simpleEqs.bodyWithDependencies(rho)(3) }
    }
    
    it("correctly adds boxes") {
      val eqs = simpleEqs.withBoxes({ case _ => box }, false)
      assertResult(0) { eqs.body(rho)(0) }
      assertResult(2) { eqs.body(rho)(1) }
      assertResult(4) { eqs.body(rho)(2) }
      assertResult(9) { eqs.body(rho)(3) }
    }
  }

}