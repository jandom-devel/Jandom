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

package it.unich.jandom.fixpoint.infinite

import scala.collection.mutable.Buffer

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks

import it.unich.jandom.fixpoint._
import it.unich.jandom.fixpoint.EquationSystem.SimpleEquationSystem
import it.unich.jandom.utils.IterableFunction

/**
 * Test solvers for finite equation systems.
 */
class InfiniteEquationSystemTest extends FunSpec with PropertyChecks {
  import EquationSystem._

  val simpleEqs = SimpleEquationSystem[Int, Int](
    body = { (rho: Int => Int) =>
      x: Int =>
        if (x % 2 == 0)
          rho(rho(x)) max x / 2
        else {
          val n = (x - 1) / 2
          rho(6 * n + 4)
        }
    },
    initial = { _ => 0}
   )

  val maxBox: Box[Int] = { _ max _ }
  val startRho = simpleEqs.initial

  type SimpleSolver[U, V] = (EquationSystem[U, V], U => V, Seq[U]) => IterableFunction[U, V]

  class EvaluationOrderListener extends FixpointSolverListenerAdapter {
    val buffer = Buffer.empty[Any]
    override def evaluated[U1, V1](rho: U1 => V1, x: U1, newval: V1) {
      buffer += x
    }
  }

  /**
   * Test solvers for the `simpleEqs` equation system when starting from the initial
   * assignment `startRho`.
   */
  def testExpectedResult(solver: SimpleSolver[Int, Int]) {
    it("gives the expected result starting from startRho with max") {
      val finalRho = solver(simpleEqs.withBoxes(maxBox, true), startRho, Seq(4))
      assertResult(Set(0, 1, 2, 4))(finalRho.keySet)
      assertResult(2)(finalRho(1))
      assertResult(2)(finalRho(2))
      assertResult(2)(finalRho(4))
    }
  }

  describe("The standard bodyWithDependencies method") {
    it("returns the correct dependencies") {
      val (res, deps) = simpleEqs.bodyWithDependencies(startRho)(4)
      assertResult((2, Seq(4, 0))) { simpleEqs.bodyWithDependencies(startRho)(4) }
      assertResult((0, Seq(4))) { simpleEqs.bodyWithDependencies(startRho)(1) }
    }
    it("returns the same value as body") {
      forAll { (x: Int) =>
        assertResult(simpleEqs.body(startRho)(x)) { simpleEqs.bodyWithDependencies(startRho)(x)._1 }
      }
    }
  }
  
  describe("The DynamicPriorityOrdering") {
    it("puts new element first in the ordering") {
      val o = new PriorityWorkListSolver.DynamicPriority[Int]
      o.lteq(1,1)
      assert(o.lt(2,1))
      assert(o.lt(3,2))
      assert(o.lt(3,1))
      assert(o.lt(-10,2))
    }
  }

  describe("The WorkListSolver") {    
    testExpectedResult(WorkListSolver(_, _, _))    
  }

  describe("The PriorityWorkListSolver") {    
    testExpectedResult(PriorityWorkListSolver(_, _, _))    
  }
}
