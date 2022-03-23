/**
  * Copyright 2016, 2017, 2018 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.jandom.benchmark.FASTLoader
import it.unich.jandom.domains.numerical.ParallelotopeRationalDomain
import it.unich.jandom.targets.Parameters
import it.unich.jandom.targets.lts._
import it.unich.jandom.targets.parameters._
import it.unich.scalafix.Assignment

/**
  * This program compares standard (legacy) analyzer with the new one based on Scalafix for
  * the LTS target. The aim is to compare both precision of results and speed. This should be used to
  * guide the progressive removal of legacy analyzers.
  */
object EQSLegacyFASTComparison extends App with FASTLoader {

  val dom = ParallelotopeRationalDomain()

  def compareResults(locations: Seq[Location], ann1: Assignment[Location, dom.Property], ann2: Assignment[Location, dom.Property]) = {
    var lt, eq, gt, un = 0
    for (l <- locations) {
      val result = ann1(l) tryCompareTo ann2(l)
      (result: @unchecked) match {
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
    (eq, lt, gt, un)
  }

  val paramsList = Seq(
    ("standard", new Parameters[LTS] {
      val domain: dom.type = dom
    }),
    ("kleene standard", new Parameters[LTS] {
      val domain: dom.type = dom
      iterationStrategy = IterationStrategy.Kleene
    }),
    ("all widening", new Parameters[LTS] {
      val domain: dom.type = dom
      wideningLocation = WideningNarrowingLocation.All
      narrowingLocation = WideningNarrowingLocation.All
    }),
    ("kleene all widening", new Parameters[LTS] {
      val domain: dom.type = dom
      wideningLocation = WideningNarrowingLocation.All
      narrowingLocation = WideningNarrowingLocation.All
      iterationStrategy = IterationStrategy.Kleene
    })
  )

  for ((name, params) <- paramsList) {
    var timeEQS = 0L
    var timeLTS = 0L
    var timeTemp = 0L
    var globaleq, globallt, globalgt, globalun = 0
    for (lts <- ltss) {
      timeTemp = java.lang.System.currentTimeMillis()
      val eqsAdapter = lts.toEQS(dom)
      val eqs = eqsAdapter.transformed
      val paramsEqs: eqs.Parameters = new eqs.Parameters {
        val domain = dom
        wideningLocation = params.wideningLocation
        narrowingLocation = params.narrowingLocation
        iterationStrategy = params.iterationStrategy
      }
      val res1 = eqsAdapter.pullbackAnnotation(eqs.analyze(paramsEqs))
      timeEQS += (java.lang.System.currentTimeMillis() - timeTemp)
      timeTemp = java.lang.System.currentTimeMillis()
      val res2 = lts.analyze(params)
      timeLTS += (java.lang.System.currentTimeMillis() - timeTemp)
      val comparison = compareResults(lts.locations, res1, res2)
      globaleq += comparison._1
      globallt += comparison._2
      globalgt += comparison._3
      globalun += comparison._4
      if (comparison._2 != 0 || comparison._3 != 0 || comparison._4 != 0)
        println(s"Found differences in ${lts.name}")
    }
    println(s"\n-------------------")
    println(s"$name EQS vs Legacy")
    println(s"Time ${timeEQS}ms vs ${timeLTS}ms")
    println("Equal : " + globaleq)
    println("First Better: " + globallt)
    println("Second Better: " + globalgt)
    println("Uncomparable: " + globalun)
  }
}
