/**
 * Copyright 2014, 2016 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom

import java.io.{ File, FileReader }

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.narrowings.DefaultNarrowing
import it.unich.jandom.parsers.FastParser
import it.unich.jandom.ppfactories._
import it.unich.jandom.ppfactories.PPFactory.ConstantFactory
import it.unich.jandom.targets.IterationStrategy
import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.widenings.DefaultWidening

/**
 * Example program using ''Jandom'' to analyze the Alice benchmarks and
 * compare the results with different parameters. In this moment, it compares
 * the result of the analyisis with standard Kleene iteration and worklist
 * based ones.
 */
object JandomFasterBench extends App {

  def fastModelAnalyze(model: File) = {
    println(s"------>${model}")

    val source = new FileReader(model)
    val parsed = FastParser().parse(source)
    source.close()
    val program = parsed.get
    val params = new targets.Parameters[LTS] { val domain = BoxDoubleDomain(false) }

    // We specify some parameters for the analysis, although these are the standard ones.
    params.wideningFactory = DefaultWidening
    params.narrowingFactory = DefaultNarrowing
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
      println("DIFFERENT BEHAVIOURS: " + model)
      println(s"Times:  ${tann1} vs  ${tann2}")
      println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
      println("BETTER 1: " + program.locations.filter(loc => ann(loc) < ann2(loc)).map(_.name).mkString(", "))
      println("BETTER 2: " + program.locations.filter(loc => ann(loc) > ann2(loc)).map(_.name).mkString(", "))
      println(program.mkString(ann))
      println(program.mkString(ann2))
    }
  }

  val resources = getClass.getResource("/fast/").toURI;

  // This analyzes all models (does not terminate for descending2 with
  for (model <- new File(resources).listFiles()) fastModelAnalyze(model)

  // This is if we want to analyze a specificic model
  // fastModelAnalyze(new File(resources.resolve("descending.fst")))
}
