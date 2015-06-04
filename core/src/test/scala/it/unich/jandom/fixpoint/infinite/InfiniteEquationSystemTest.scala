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
class InfiniteEquationSystemTest extends FunSpec with PropertyChecks {

  object simpleEqs extends EquationSystem {
    type Unknown = Int
    type Value = Int
    def apply(rho: Assignment): Assignment = { x =>
      if (x % 2 == 0) 
        rho(rho(x)) max x/2
      else {
        val n = (x-1)/2
        rho(6*n + 4)
      }        
    }    
  }
   
  /*
  implicit object Listener extends FixpointSolverListener {
    def evaluated[EQS <: EquationSystem](rho: EQS#Assignment, u: EQS#Unknown, newval: EQS#Value) { println(s"evaluated ${u} with value ${newval}") }
    def initialized[EQS <: EquationSystem](rho: EQS#Assignment) { println("initialized") }
  }
  */
  
  val maxBox = Box[Int] { _ max _ }
  val startRho = { (x: Int) => 0 }

  type SimpleSolver[EQS <: EquationSystem with Singleton] = (Iterable[EQS#Unknown], EQS#Assignment, EQS#BoxAssignment) => EQS#Assignment

  /**
   * Test solvers for the `simpleEqs` equation system when starting from the initial
   * assignment `startRho`.
   */
  def testExpectedResult(solver: LocalFixpointSolver[simpleEqs.type], m: PMap)(implicit conv: Function1[solver.wanted.type +: solver.start.type +: solver.boxes.type +: m.type, solver.Parameters]) {
    import solver._
    it("gives the expected result starting from startRho with max") {
      val params = (wanted --> Seq(4)) +: (start --> startRho) +: (boxes--> maxBox) +: m
      val finalRho = solver(params)
      assertResult(2)(finalRho(1))
      assertResult(2)(finalRho(2))
      assertResult(2)(finalRho(4))      
    }
  }

  describe("The WorkListSolver") {
    testExpectedResult(new WorkListSolver(simpleEqs), PMap.empty) 
  }
  describe("The PriorityWorkListSolver") { 
    val pwsolver = new PriorityWorkListSolver(simpleEqs)
    testExpectedResult(pwsolver, (pwsolver.priorities --> UFactory.memoize(new PriorityWorkListSolver.DynamicPriority)) +: PMap.empty)
  }
}
