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
import it.unich.jandom.domains.numerical.ppl.PPLDomain
import it.unich.jandom.fixpoint.Driver._
import it.unich.jandom.fixpoint.finite.PriorityWorkListSolver
import it.unich.jandom.fixpoint.FixpointSolverListener.PerformanceListener
import it.unich.jandom.fixpoint.structured.StructuredDriver
import it.unich.jandom.utils.PMaps._

import parma_polyhedra_library.C_Polyhedron

/**
 * An example application which compares the precision of different analysis for Alice benchmarks.
 */
object FASTComparison extends App with FASTLoader {
  //val dom = PPLDomain[C_Polyhedron]
  val dom = BoxDoubleDomain()

  //val delayedNarrowing = Narrowings.Delayed(Narrowings.Intersection, 2, Narrowings.Default)
  val anarrowing = Narrowings.Default
  val box = Updates.Combine(Widenings.Default, anarrowing)
  val basebox = (boxstrategy --> BoxStrategy.Mixed) +: (update --> box) +: (solver --> Solver.PriorityWorkListSolver) +: (narrowing --> anarrowing) +: PMap.empty

  val parameters = Seq(
    ("standard", (narrowing --> anarrowing) +: PMap.empty),
    ("localized", (narrowing --> anarrowing) +: (boxscope --> BoxScope.Localized) +: PMap.empty),
    ("mixed", basebox),
    ("mixed localized", (boxscope --> BoxScope.Localized) +: basebox),
    ("mixed localized restart", (boxscope --> BoxScope.Localized) +: (restartstrategy --> true) +: basebox))

  val results = (for (lts <- ltss; eqs = lts.toEQS(dom); (name, p) <- parameters) yield {
    val l = new PerformanceListener
    val param = (listener --> l) +: p
    ((lts, name), (StructuredDriver(dom)(eqs, param), l.evaluations))
  }).toMap

  for ((name, _) <- parameters) {
    var numiters = 0
    for (l <- ltss) numiters += results((l, name))._2
    println(s"solver ${name} iterations ${numiters}")
  }

  for (i <- 0 until parameters.size; j <- i + 1 until parameters.size) {
    val name1 = parameters(i)._1
    val name2 = parameters(j)._1
    var globalun, globaleq, globallt, globalgt = 0
    for (lts <- ltss) {
      var lt, eq, gt, un = 0
      val ann1 = results((lts, name1))._1
      val ann2 = results((lts, name2))._1
      for (l <- lts.locations) {
        val result = ann1(l) tryCompareTo ann2(l)
        result match {
          case None =>
            un += 1
          //println(s"location ${l}: ${ann1(l)} vs ${ann2(l)}")
          case Some(0) => eq += 1
          case Some(x) if x < 0 =>
            lt += 1
          //println(s"location ${l}: ${ann1(l)} vs ${ann2(l)}")
          case Some(x) if x > 0 =>
            gt += 1
          //println(s"location ${l}: ${ann1(l)} vs ${ann2(l)}")
        }
      }
      /*
      println(s"${name1} vs ${name2} for ${lts.name}")
      println("Uncomparable: " + un)
      println("Equal : " + eq)
      println("First Better: " + lt)
      println("Second Better: " + gt)
      */
      globaleq += eq
      globallt += lt
      globalgt += gt
      globalun += un
    }
    // for comparison, the old solver integrated in the LTS class has 1170 evaluations for worklist based analysis and 1706 evaluations for Kleene.
    println(s"\n-------------------")
    println(s"${name1} vs ${name2}")
    println("Uncomparable: " + globalun)
    println("Equal : " + globaleq)
    println("First Better: " + globallt)
    println("Second Better: " + globalgt)
  }

  for (lts <- ltss; if lts.name.indexOf("amato") >= 0) {
    println(lts.name)
    for ((name, _) <- parameters) {
      print(s"Solver ${name} -> ")
      val result = results((lts, name))
      for (l <- lts.locations) print(s"${l} : ${result._1(l)} ")
      println("")
    }
  }
}
