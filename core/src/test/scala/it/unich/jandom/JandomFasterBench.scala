/**
  * Copyright 2014, 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom

import it.unich.jandom.benchmark.FASTLoader
import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.domains.numerical.ppl.PPLDomainMacro
import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.targets.parameters.IterationStrategy
import it.unich.jandom.targets.parameters.NarrowingSpecs._
import it.unich.jandom.targets.parameters.WideningSpecs._
import parma_polyhedra_library.C_Polyhedron

/**
  * Example program using ''Jandom'' to analyze the Alice benchmarks and
  * compare the results with different parameters. In this moment, it compares
  * the result of the analyisis with standard Kleene iteration and worklist
  * based ones.
  */
object JandomFasterBench extends App with FASTLoader {

  def fastModelAnalyze(program: LTS) {
    println(s"------> ${program.name}")

    val params = new targets.Parameters[LTS] {
      val domain: NumericalDomain = PPLDomainMacro[C_Polyhedron]
    }

    // We specify some parameters for the analysis, although these are the standard ones.
    params.widening = DefaultWidening
    params.narrowing = DefaultNarrowing
    params.iterationStrategy = IterationStrategy.Worklist
    //params.debugWriter = new java.io.PrintWriter(System.out)
    program.analyze(params) // warmup JVM

    val t1 = System.currentTimeMillis
    val ann = program.analyze(params)
    val tann1 = System.currentTimeMillis - t1

    // disable narrowing, if we want to check the improvements we obtain
    // params.narrowingLocation = WideningNarrowingLocation.None
    params.iterationStrategy = IterationStrategy.Kleene
    val t2 = System.currentTimeMillis
    val ann2 = program.analyze(params)
    val tann2 = System.currentTimeMillis - t2

    // params.debugWriter.flush()

    if (program.locations exists (loc => ann(loc) != ann2(loc))) {
      println("DIFFERENT BEHAVIOURS: " + program.name)
      println(s"Times:  $tann1 vs  $tann2")
      println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
      println("BETTER 1: " + program.locations.filter(loc => ann(loc) < ann2(loc)).map(_.name).mkString(", "))
      println("BETTER 2: " + program.locations.filter(loc => ann(loc) > ann2(loc)).map(_.name).mkString(", "))
      println(program.mkString(ann))
      println(program.mkString(ann2))
    }
  }

  for (lts <- ltss) fastModelAnalyze(lts)
}
