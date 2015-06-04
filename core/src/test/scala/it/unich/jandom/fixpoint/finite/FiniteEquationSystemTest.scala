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

package it.unich.jandom.fixpoint.finite

import it.unich.jandom.fixpoint._
import it.unich.jandom.utils.PMaps._
import it.unich.jandom.utils.Relation

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import scala.collection.immutable.HashMap

/**
 * Test solvers for finite equation systems.
 */
class FiniteEquationSystemTest extends FunSpec with PropertyChecks {

  import HierarchicalOrdering._

  object simpleEqs extends FiniteEquationSystem {
    type Unknown = Int
    type Value = Double
    def apply(rho: Assignment): Assignment = {
      case 0 => rho(0)
      case 1 => (rho(0) max rho(2)) min rho(3)
      case 2 => rho(1) + 1
      case 3 => rho(3)
    }
    val unknowns = Set(0, 1, 2, 3)
    val infl = Relation(Map(0 -> Set(0, 1, 2), 1 -> Set(2), 2 -> Set(1), 3 -> Set(1, 3)))
  }

  val simpleEqsStrategy = HierarchicalOrdering[Int](Left, Val(0), Left, Val(1), Val(2), Val(3), Right, Right)

  val wideningBox = Box[Double] { (x1, x2) => if (x2 > x1) Double.PositiveInfinity else x1 }
  val maxBox = Box[Double] { _ max _ }
  val lastBox = Box[Double] { (x1, x2) => x2 }

  val startRho: Int => Double = { (x: Int) => if (x == 3) 10.0 else 0.0 }

  type SimpleSolver[EQS <: FiniteEquationSystem with Singleton] = (EQS#Assignment, EQS#BoxAssignment) => EQS#Assignment

  /**
   * Tests whether solving `eqs` equation system always returns a correct result. Should be used only for solvers which are
   * guaranteed to terminate with the given box assignment.
   */
  def testCorrectness(solver: FixpointSolver[simpleEqs.type], m: PMap, boxAssignment: simpleEqs.BoxAssignment)
    (implicit conv: Function1[solver.start.type +: solver.boxes.type +: m.type, solver.Parameters], values: Arbitrary[solver.eqs.Value]) = {
    import solver._
    val startRhosList = Gen.listOfN(eqs.unknowns.size, values.arbitrary)
    val startRhos = startRhosList map { (l) => HashMap(eqs.unknowns.toSeq zip l: _*) }
    forAll(startRhos) { startEnv =>
      val params = (solver.start --> startEnv) +: (solver.boxes --> boxAssignment) +: m
      val finalEnv = solver(params)
      for (x <- eqs.unknowns)
        assert(finalEnv(x) === boxAssignment(x)(finalEnv(x), eqs(finalEnv)(x)))
    }
  }

  /**
   * Test solvers for the `simpleEqs` equation system when starting from the initial
   * assignment `startRho`.
   */
  def testExpectedResult(solver: FixpointSolver[simpleEqs.type], m: PMap)(implicit conv: Function1[solver.start.type +: solver.boxes.type +: m.type, solver.Parameters]) {
    import solver._

    it("gives the expected result starting from startRho with last") {
      val params = (start --> startRho) +: (boxes --> lastBox) +: m
      val finalRho = solver(params)
      assert(finalRho(0) === 0.0)
      assert(finalRho(1) === 10.0)
      assert(finalRho(2) === 11.0)
      assert(finalRho(3) === 10.0)

    }

    it("gives the expected result starting from startRho with max") {
      val params = (start --> startRho) +: (boxes --> maxBox) +: m
      val finalRho = solver(params)
      assert(finalRho(0) === 0.0)
      assert(finalRho(1) === 10.0)
      assert(finalRho(2) === 11.0)
      assert(finalRho(3) === 10.0)
    }

    it("gives the expected result starting from startRho with widening") {
      val params = (start --> startRho) +: (boxes --> wideningBox) +: m
      val finalRho = solver(params)
      assert(finalRho(0) === 0.0)
      assert(finalRho(1) === Double.PositiveInfinity)
      assert(finalRho(2) === Double.PositiveInfinity)
      assert(finalRho(3) === 10.0)
    }

    it("always returns a box solution with widening") {      
      testCorrectness(solver, m, wideningBox)
    }
  }

  describe("The dfOrdering method") {
    it("returns a depth first ordering") {
      val o = DFOrdering(simpleEqs.infl)(0, 3)
      val combination = for (x <- 0 to 3; y <- 0 to 3) yield (x, y)
      val realorder = Seq(3, 0, 1, 2)
      val table = Table(("x", "y"), combination: _*)
      forAll(table) { (x, y) => assertResult(o.compare(x, y)) { realorder.indexOf(x) compare realorder.indexOf(y) } }
    }
  }
  describe("The RoundRobinSolver") { testExpectedResult(new RoundRobinSolver(simpleEqs), PMap.empty) }
  describe("The WorkListSolver") { testExpectedResult(new WorkListSolver(simpleEqs), PMap.empty) }
  describe("The KleeneSolver") { testExpectedResult(new KleeneSolver(simpleEqs), PMap.empty) }
  val pwsolver = new PriorityWorkListSolver(simpleEqs)
  describe("The PriorityWorkListSolver") { testExpectedResult(pwsolver, (pwsolver.ordering --> simpleEqsStrategy) +: PMap.empty) }
  val hosolver = new HierarchicalOrderingSolver(simpleEqs)
  describe("The HierarchicalOrderingSolver") { testExpectedResult(hosolver, (hosolver.ordering --> simpleEqsStrategy) +: PMap.empty) }
}
