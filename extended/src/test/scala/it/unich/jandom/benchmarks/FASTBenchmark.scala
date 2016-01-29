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

package it.unich.jandom.benchmarks

import com.google.caliper.SimpleBenchmark

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.targets.Parameters
import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.targets.lts.Location
import it.unich.scalafix.Box.apply
import it.unich.scalafix.FixpointSolver._
import it.unich.scalafix.finite.FiniteFixpointSolver

/**
 * This is a program which analyzes the Alice benchmarks with different settings and compares the execution time.
 */
class FASTBenchmark extends SimpleBenchmark with FASTLoader {

  val dom = BoxDoubleDomain()

  implicit val scalafixDomain = dom.ScalaFixDomain
  val wideningBox = { (x: dom.Property, y: dom.Property) => x widening y }
  val narrowingBox = { (x: dom.Property, y: dom.Property) => x narrowing y }
  val CC77 = FiniteFixpointSolver.CC77[Location, dom.Property](Solver.WorkListSolver, wideningBox, narrowingBox)

  def timeLTS(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val params = new Parameters[LTS] { val domain = dom }
        val ann = lts.analyze(params)
      }
    }
  }

  def timeEQSKleene(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQS(dom)
        val ann = FiniteFixpointSolver(eqs, CC77.copy(solver = Solver.KleeneSolver))
      }
    }
  }

  def timeEQSRoundRobin(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQS(dom)
        val ann = FiniteFixpointSolver(eqs, CC77.copy(solver = Solver.RoundRobinSolver))
      }
    }
  }

  def timeEQSDefault(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQS(dom)
        val ann = FiniteFixpointSolver(eqs, CC77)
      }
    }
  }

  def timeEQSLocalized(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQS(dom)
        val ann = FiniteFixpointSolver(eqs, CC77.copy(boxscope = BoxScope.Localized))
      }
    }
  }

  def timeEQSMixedLocalized(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQS(dom)
        val ann = FiniteFixpointSolver(eqs, CC77.copy(boxscope = BoxScope.Localized, boxstrategy = BoxStrategy.Warrowing))
      }
    }
  }

}
