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
import it.unich.jandom.targets.lts.Location
import it.unich.scalafix.Box.apply
import it.unich.scalafix.FixpointSolver._
import it.unich.scalafix.FixpointSolverListener.PerformanceListener
import it.unich.scalafix.finite.FiniteFixpointSolver

/**
 * An example application which compares the precision of different analysis for Alice benchmarks.
 */
object FASTComparison extends App with FASTLoader {
  //val dom = PPLDomain[C_Polyhedron]
  val dom = BoxDoubleDomain()

  implicit val scalafixDomain = dom.ScalaFixDomain
  val widening = { (x: dom.Property, y: dom.Property) => x widening y }
  val narrowing = { (x: dom.Property, y: dom.Property) => x narrowing y }

  val CC77 = FiniteFixpointSolver.CC77[Location, dom.Property](Solver.WorkListSolver, widening, narrowing)
  val SCP = CC77.copy[Location, dom.Property](solver = Solver.PriorityWorkListSolver, boxscope = BoxScope.Standard, boxstrategy = BoxStrategy.Warrowing)

  val parameters = Seq(
    ("standard", CC77),
    ("localized", CC77.copy(boxscope = BoxScope.Localized)),
    ("mixed", SCP),
    ("mixed localized", SCP.copy(boxscope = BoxScope.Localized)),
    ("mixed localized restart", SCP.copy(boxscope = BoxScope.Localized, restartstrategy = true)))

  val results = (for (lts <- ltss; eqs = lts.toEQS(dom); (name, p) <- parameters) yield {
    val l = new PerformanceListener
    val param = p.copy(listener = l)
    ((lts, name), (FiniteFixpointSolver(eqs, param), l.evaluations))
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
