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

package it.unich.jandom.fixpoint.finite

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks

import it.unich.jandom.fixpoint._
import it.unich.jandom.utils.Relation

/**
 * Test solvers for finite equation systems.
 */
class FiniteEquationSystemTest extends FunSpec with PropertyChecks {
  import HierarchicalOrdering._

  val simpleEqs = FiniteEquationSystem[Int, Double](
    body = { (rho: Int => Double) =>
      x: Int =>
        x match {
          case 0 => rho(0)
          case 1 => (rho(0) max rho(2)) min rho(3)
          case 2 => rho(1) + 1
          case 3 => rho(3)
        }
    },
    unknowns = Set(0, 1, 2, 3),
    inputUnknowns = Set(0, 1, 2, 3),
    infl = Relation(Map(0 -> Set(0, 1, 2), 1 -> Set(2), 2 -> Set(1), 3 -> Set(1, 3))))

  val simpleEqsStrategy = HierarchicalOrdering[Int](Left, Val(0), Left, Val(1), Val(2), Val(3), Right, Right)
  val wideningBox: Box[Double] = { (x1, x2) => if (x2 > x1) Double.PositiveInfinity else x1 }
  val maxBox: Box[Double] = { _ max _ }
  val lastBox: Box[Double] = { (x1, x2) => x2 }
  val timesBox: Box[Double] = { _ * _ }

  val startRho = { (x: Int) => if (x == 3) 10.0 else 0.0 }

  type SimpleSolver[U, V] = (FiniteEquationSystem[U, V], U => V) => (U => V)

  /**
   * Tests whether solving `eqs` equation system always returns a correct result. Should be used only for solvers which are
   * guaranteed to terminate with the given box assignment.
   */
  def testCorrectness[U, V](eqs: FiniteEquationSystem[U, V], solver: SimpleSolver[U, V])(implicit values: Arbitrary[V]) = {
    import solver._
    val startRhosList = Gen.listOfN(eqs.unknowns.size, values.arbitrary)
    val startRhos = startRhosList map { (l) => Map(eqs.unknowns.toSeq zip l: _*) }
    forAll(startRhos) { start =>
      val finalEnv = solver(eqs, start)
      for (x <- eqs.unknowns)
        assert(finalEnv(x) === eqs.body(finalEnv)(x))
    }
  }

  /**
   * Test solvers for the `simpleEqs` equation system when starting from the initial
   * assignment `startRho`.
   */
  def testExpectedResult(solver: SimpleSolver[Int, Double]) {
    import solver._

    it("gives the expected result starting from startRho with last") {
      val finalRho = solver(simpleEqs.withBoxes(lastBox, true), startRho)
      assert(finalRho(0) === 0.0)
      assert(finalRho(1) === 10.0)
      assert(finalRho(2) === 11.0)
      assert(finalRho(3) === 10.0)
    }

    it("gives the expected result starting from startRho with max") {
      val finalRho = solver(simpleEqs.withBoxes(maxBox, true), startRho)
      assert(finalRho(0) === 0.0)
      assert(finalRho(1) === 10.0)
      assert(finalRho(2) === 11.0)
      assert(finalRho(3) === 10.0)
    }

    it("gives the expected result starting from startRho with widening") {
      val finalRho = solver(simpleEqs.withBoxes(wideningBox, true), startRho)
      assert(finalRho(0) === 0.0)
      assert(finalRho(1) === Double.PositiveInfinity)
      assert(finalRho(2) === Double.PositiveInfinity)
      assert(finalRho(3) === 10.0)
    }

    it("always returns a box solution with widening") {
      testCorrectness(simpleEqs.withBoxes(wideningBox, true), solver)
    }
  }

  describe("A finite equation system") {
    val rho: PartialFunction[Int, Double] = { case x => x.toDouble }

    it("computes r.h.s. according to its body function") {
      assertResult(0) { simpleEqs.body(rho)(0) }
      assertResult(2) { simpleEqs.body(rho)(1) }
      assertResult(2) { simpleEqs.body(rho)(2) }
      assertResult(3) { simpleEqs.body(rho)(3) }
    }

    it("correctly infers dependencies") {
      assertResult((0, Seq(0))) { simpleEqs.withDependencies(rho)(0) }
      assertResult((2, Seq(0, 2, 3))) { simpleEqs.withDependencies(rho)(1) }
      assertResult((2, Seq(1))) { simpleEqs.withDependencies(rho)(2) }
      assertResult((3, Seq(3))) { simpleEqs.withDependencies(rho)(3) }
    }

    it("correctly adds boxes") {
      val eqs = simpleEqs.withBoxes(timesBox, false)
      assertResult(0) { eqs.body(rho)(0) }
      assertResult(2) { eqs.body(rho)(1) }
      assertResult(4) { eqs.body(rho)(2) }
      assertResult(9) { eqs.body(rho)(3) }
    }
  }

  describe("The RoundRobinSolver") { testExpectedResult(RoundRobinSolver(_, _)) }
  describe("The WorkListSolver") { testExpectedResult(WorkListSolver(_, _)) }
  describe("The KleeneSolver") { testExpectedResult(KleeneSolver(_, _)) }
  describe("The PriorityWorkListSolver") { testExpectedResult(PriorityWorkListSolver(_, _, simpleEqsStrategy)) }
  describe("The HierarchicalOrderingSolver") { testExpectedResult(HierarchicalOrderingSolver(_, _, simpleEqsStrategy)) }
}
