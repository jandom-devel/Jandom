/**
  * Copyright 2018 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.jandom.benchmark.SLILLoader
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.targets.parameters.NarrowingStrategy
import it.unich.jandom.targets.slil.SLILTarget

/**
  * This program compares standard (legacy) analyzer with the new one based on Scalafix for
  * the SLIL target. The aim is to compare both precision of results and speed.
  */
object EQSLegacySLILComparison extends App with SLILLoader {

  private def compareResults(programPoints: Iterable[SLILTarget#ProgramPoint], ann1: SLILTarget#ProgramPoint => dom.Property,
                             ann2: SLILTarget#ProgramPoint => dom.Property) = {
    var lt, eq, gt, un = 0
    for (l <- programPoints) {
      val result = ann1(l) tryCompareTo ann2(l)
      result match {
        case None =>
          un += 1
        case Some(0) => eq += 1
        case Some(x) if x < 0 =>
          lt += 1
        case Some(x) if x > 0 =>
          gt += 1
      }
    }
    (eq, lt, gt, un)
  }

  val dom = BoxDoubleDomain()

  var timeEQS = 0.0
  var timeLTS = 0.0
  var timeTemp = 0.0
  var globaleq, globallt, globalgt, globalun = 0


  for ((f, p) <- slils) {
    println(f)
    val params = new p.Parameters {
      val domain: dom.type = dom
      narrowingStrategy = NarrowingStrategy.Separate
      //wideningScope = WideningScope.Random
      //iterationStrategy =  IterationStrategy.Worklist
      //wideningLocation = WideningNarrowingLocation.Loop
      //narrowingLocation = WideningNarrowingLocation.Loop
    }
    val eqsAdapter = p.toEQS(dom)
    val eqs = eqsAdapter.transformed
    val params2 = new eqs.Parameters {
      val domain: dom.type = dom
      iterationStrategy = params.iterationStrategy
      wideningLocation = params.wideningLocation
      narrowingLocation = params.narrowingLocation
      wideningScope = params.wideningScope
    }

    timeTemp = java.lang.System.currentTimeMillis()
    val reseqs = eqs.analyze(params2)
    timeEQS += (java.lang.System.currentTimeMillis() - timeTemp)
    val res1 = eqsAdapter.pullbackAnnotation(reseqs)
    timeTemp = java.lang.System.currentTimeMillis()
    val res2 = p.analyze(params)
    timeLTS += (java.lang.System.currentTimeMillis() - timeTemp)

    /*
    for (u <- res1.keys) print(s"$u -> ${res1(u)} ")
    print("\n")
    for (u <- res2.keys) print(s"$u -> ${res2(u)} ")
    print("\n")
    */

    val (eq, lt, gt, un) = compareResults(res1.keys, res1, res2)

    globaleq += eq
    globallt += lt
    globalgt += gt
    globalun += un
    if (lt != 0 || gt != 0 || un != 0) {
      println(s"Found differences in $f")
      println("--> EQS")
      println(p.mkString(res1))
      println("--> Legacy")
      println(p.mkString(res2))
    }

  }
  println(s"\n-------------------")
  println(s"EQS vs Legacy")
  println(s"Time ${timeEQS}ms vs ${timeLTS}ms")
  println("Equal : " + globaleq)
  println("First Better: " + globallt)
  println("Second Better: " + globalgt)
  println("Uncomparable: " + globalun)
}
