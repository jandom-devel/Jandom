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

package it.unich.jandom.fixpoint

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scala.collection.immutable.HashMap

/**
 * Test solvers for finite equation systems.
 */
class FiniteEquationSystemTest extends FunSpec with PropertyChecks {

  import IterativeStrategy._

  object simpleEqs extends FiniteEquationSystem {
    type Unknown = Int
    type Value = Double
    def apply(rho: Assignment): Assignment = {
      case 0 => rho(0)
      case 1 => (rho(0) max rho(2)) min rho(3)
      case 2 => rho(1) + 1
      case 3 => rho(3)
    }
    val unknowns = Seq(0, 1, 2, 3)
    def infl(x: Int) = x match {
      case 0 => Seq(0, 1, 2)
      case 1 => Seq(2)
      case 2 => Seq(1)
      case 3 => Seq(1, 3)
    }
  }

  val simpleEqsStrategy = IterativeStrategy[Int](Left, El(0), Left, El(1), El(2), El(3), Right, Right)

  val wideningBox: simpleEqs.Box = { (x1, x2) => if (x2 > x1) Double.PositiveInfinity else x1 }
  val maxBox: simpleEqs.Box = { _ max _ }
  val lastBox: simpleEqs.Box = { (x1, x2) => x2 }

  val allMax: Int => simpleEqs.Box = { _ => maxBox }
  val allWiden: Int => simpleEqs.Box = { _ => wideningBox }
  val allLast: Int => simpleEqs.Box = { _ => lastBox }

  val startRho: Int => Double = { (x: Int) => if (x == 3) 10.0 else 0.0 }

  /**
   * Tests whether solving `eqs` equation system always returns a correct result. Should be used only for solvers which are
   * guaranteed to terminate with the given box assignment.
   */
  def testCorrectness(solver: FixpointSolver[_ <: FiniteEquationSystem])(boxes: solver.eqs.Unknown => solver.eqs.Box)(implicit values: Arbitrary[solver.eqs.Value]) = {
    val startRhosList = Gen.listOfN(solver.eqs.unknowns.size, values.arbitrary)
    val startRhos = startRhosList map { (l) => HashMap(solver.eqs.unknowns zip l: _*) }
    it("always returns a box solution") {
      forAll(startRhos) { startEnv =>
        val finalEnv = solver(startEnv, boxes)
        for (x <- solver.eqs.unknowns)
          assert(finalEnv(x) === boxes(x)(finalEnv(x), solver.eqs(finalEnv)(x)))
      }
    }
  }

  /**
   * Test solvers for the `simpleEqs` equation system when starting from the initial
   * assignment `startRho`.
   */
  def testExpectedResult(solver: FixpointSolver[simpleEqs.type]) = {
    describe(s"${solver.name} with last") {
      it("gives the expected result starting from startRho") {
        val finalRho = solver(startRho, allLast)
        assert(finalRho(0) === 0.0)
        assert(finalRho(1) === 10.0)
        assert(finalRho(2) === 11.0)
        assert(finalRho(3) === 10.0)
      }
    }

    describe(s"${solver.name} with max") {
      it("gives the expected result starting from startRho") {
        val finalRho = solver(startRho, allMax)
        assert(finalRho(0) === 0.0)
        assert(finalRho(1) === 10.0)
        assert(finalRho(2) === 11.0)
        assert(finalRho(3) === 10.0)
      }
    }

    describe(s"${solver.name} with widening") {
      it("gives the expected result starting from startRho") {
        val finalRho = solver(startRho, allWiden)
        assert(finalRho(0) === 0.0)
        assert(finalRho(1) === Double.PositiveInfinity)
        assert(finalRho(2) === Double.PositiveInfinity)
        assert(finalRho(3) === 10.0)
      }
    }

    describe(s"${solver.name} with widening") {
      testCorrectness(solver)(allWiden)
    }
  }

  testExpectedResult(RoundRobinSolver(simpleEqs))
  testExpectedResult(WorkListSolver(simpleEqs))
  testExpectedResult(IterativeStrategySolver(simpleEqs)(simpleEqsStrategy))
}
