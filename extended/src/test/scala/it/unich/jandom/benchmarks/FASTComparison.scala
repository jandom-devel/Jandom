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

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.fixpoint.Driver._
import it.unich.jandom.fixpoint.FixpointSolverListener.PerformanceListener
import it.unich.jandom.fixpoint.structured.StructuredDriver
import it.unich.jandom.utils.PMaps._

/**
 * An example application which compares the precision of different analysis for Alice benchmarks.
 */
object FASTComparison extends App with FASTLoader {
  val dom = BoxDoubleDomain()

  var globaliter1, globaliter2, globalun, globaleq, globallt, globalgt = 0
  for (lts <- ltss) {
    println("----------------------------")
    println(lts.name)
    val eqs = lts.toEQS(dom)
    
    val l1 = new PerformanceListener 
    val l2 = new PerformanceListener
    val ann1 = StructuredDriver(dom)(eqs, (listener --> l1) +: PMap.empty)
    val ann2 = StructuredDriver(dom)(eqs, (listener --> l2) +: (boxstrategy --> BoxStrategy.Mixed) +: (boxscope --> BoxScope.Localized) +: (solver --> Solver.PriorityWorkListSolver) +: PMap.empty)
    var lt, eq, gt, un = 0
    for (l <- lts.locations) {
      val result = ann1(l) tryCompareTo ann2(l)
      result match {
        case None => un += 1
        case Some(0) => eq += 1
        case Some(x) if x < 0 => lt += 1
        case Some(x) if x > 0 => gt += 1
      }
    }
    globaliter1 += l1.evaluations
    globaliter2 += l2.evaluations
    globaleq += eq
    globallt += lt
    globalgt += gt
    globalun += un
    println(s"Iterations: ${l1.evaluations} vs ${l2.evaluations}")
    println("Uncomparable: " + un)
    println("Equal : " + eq)
    println("First Better: " + lt)
    println("Second Better: " + gt)
  }  

  // for comparison, the old solver integrated in the LTS class has 1170 evaluations for worklist based analysis and 1706 evaluations for Kleene.
  println("\nGlobalInfo -------------- ")
  println(s"Iterations: ${globaliter1} vs ${globaliter2}")
  println("Uncomparable: " + globalun)
  println("Equal : " + globaleq)
  println("First Better: " + globallt)
  println("Second Better: " + globalgt)
}
