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
import it.unich.jandom.domains.numerical._
import it.unich.jandom.fixpoint.structured.StructuredDriver
import it.unich.jandom.fixpoint.Driver._
import it.unich.jandom.targets.Parameters
import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.utils.PMaps._

/**
 * This is a program which analyzes the Alice benchmarks with different settings and compares the execution time.
 */
class FASTBenchmark extends SimpleBenchmark with FASTLoader {
 
  val dom = BoxDoubleDomain()
  
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
        val ann = StructuredDriver(dom)(eqs, (solver --> Solver.KleeneSolver) +: PMap.empty)
      }
    }
  }
    
  def timeEQSRoundRobin(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQS(dom)
        val ann = StructuredDriver(dom)(eqs, (solver --> Solver.RoundRobinSolver) +: PMap.empty)
      }
    }
  }
  
  def timeEQSDefault(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQS(dom)
        val ann = StructuredDriver(dom)(eqs, PMap.empty)
      }
    }
  }

  def timeEQSFlowDefault(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQSFlow(dom)
        val ann = StructuredDriver(dom)(eqs, PMap.empty)
      }
    }
  }

  def timeEQSLocalized(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQS(dom)
        val ann = StructuredDriver(dom)(eqs, (boxscope --> BoxScope.Localized) +: PMap.empty)
      }
    }
  }

  def timeEQSFlowLocalized(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQSFlow(dom)
        val ann = StructuredDriver(dom)(eqs, (boxscope --> BoxScope.Localized) +: PMap.empty)
      }
    }
  }
  
   def timeEQSMixedLocalized(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQS(dom)
        val ann = StructuredDriver(dom)(eqs, (boxstrategy --> BoxStrategy.Mixed) +: (boxscope --> BoxScope.Localized) +: (solver --> Solver.PriorityWorkListSolver) +: PMap.empty)
      }
    }
  }

  def timeEQSFlowMixedLocalized(reps: Int) {
    for (_ <- 1 to reps) {
      for (lts <- ltss) {
        val eqs = lts.toEQSFlow(dom)
        val ann = StructuredDriver(dom)(eqs, (boxstrategy --> BoxStrategy.Mixed) +: (boxscope --> BoxScope.Localized) +: (solver --> Solver.PriorityWorkListSolver) +: PMap.empty)
      }
    }
  }
}
