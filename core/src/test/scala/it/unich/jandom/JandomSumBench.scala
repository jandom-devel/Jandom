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
import it.unich.jandom.domains.DimensionFiberedProperty
import it.unich.jandom.domains.numerical._
import it.unich.jandom.domains.numerical.ppl.PPLDomain
import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.targets.parameters.WideningSpecs._
import parma_polyhedra_library.C_Polyhedron

/**
  * Example program using ''Jandom'' to analyze the Alice benchmarks and
  * compare the results with different parameters. In this moment, it compares
  * the result of the analyisis with standard Kleene iteration and worklist
  * based ones.
  */
object JandomSumBench extends App with FASTLoader {

  var totalEquals = 0
  var totalBestSum = 0
  var totalBestOther = 0
  var totalUncomparable = 0
  var totalPrograms = 0

  def CStoPolyehdra(dimension: Int, c: Seq[LinearForm]) = {
    val d = PPLDomain[C_Polyhedron]()
    c.foldLeft(d.top(dimension)) { (p: d.Property, lf: LinearForm) => p.linearInequality(lf) }
  }

  def mkString[U <: DimensionFiberedProperty[U]](program: LTS, m: scala.collection.Map[LTS#ProgramPoint, U]): String = {
    (for ((loc, prop) <- m) yield loc.name + " => " + prop.mkString(program.env.variables)).mkString(", ")
  }

  def fastModelAnalyze(program: LTS) {
    totalPrograms += 1

    println(s"------>${program.name}")
    println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))

    val params = new targets.Parameters[LTS] {
      val domain = BoxDoubleDomain()
    }
    params.widening = DelayedWidening(DefaultWidening, 3) // needed for parallelotopes
    program.analyze(params) // warmup JVM

    val t1 = System.currentTimeMillis
    val ann1 = program.analyze(params)
    val tann1 = System.currentTimeMillis - t1

    val params2 = new targets.Parameters[LTS] {
      val domain = SumBoxDoubleParallelotopeRationDomain()
    }
    params2.widening = DelayedWidening(DefaultWidening, 3) // needed for parallelotopes
    //params2.debugWriter = new java.io.PrintWriter(System.out)
    program.analyze(params2) // warmup JVM
    //params2.debugWriter.flush()

    val t2 = System.currentTimeMillis
    val ann2 = program.analyze(params2)
    val tann2 = System.currentTimeMillis - t2

    val params3 = new targets.Parameters[LTS] {
      val domain = ParallelotopeRationalDomain()
    }
    params3.widening = DelayedWidening(DefaultWidening, 3) // needed for parallelotopes
    //params3.debugWriter = tann1new java.io.PrintWriter(System.out)

    program.analyze(params3) // warmup JVM
    //params3.debugWriter.flush()

    val t3 = System.currentTimeMillis
    val ann3 = program.analyze(params3)
    val tann3 = System.currentTimeMillis - t3

    val cann1 = ann1 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cann2 = ann2 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cann3 = ann3 mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }

    println(s"Times: box $tann1  sum: $tann2  ptope: $tann3")
    print("Box: ")
    println(mkString(program, cann1))
    print("PTope: ")
    println(mkString(program, cann3))
    print("Sum: ")
    println(mkString(program, cann2))

    // SOSTITUIRE cann1 con cann3 se si vuole il confronto con i Parallelotopi.
    val comp = cann2 map { case (loc, v) => loc -> v.tryCompareTo(cann1(loc) intersection cann3(loc)) }

    //comparing sum with box
    //val comp = cann2 map { case (loc, v) => (loc -> v.tryCompareTo(cann1(loc))) }

    //comparing sum with parallelotope
    //val comp = cann2 map { case (loc, v) => (loc -> v.tryCompareTo(cann3(loc))) }

    println("COUNT EQUALS: " + comp.count(_._2 contains 0))
    println("COUNT BETTER SUM: " + comp.count(_._2 contains -1))
    println("COUNT BETTER OTHER: " + comp.count(_._2 contains 1))
    println("COUNT UNCOMPARABLES: " + comp.count(_._2.isEmpty))

    totalEquals += comp.count(_._2 contains 0)
    totalBestSum += comp.count(_._2 contains -1)
    totalBestOther += comp.count(_._2 contains 1)
    totalUncomparable += comp.count(_._2.isEmpty)

    //println("DIFFERENT BEHAVIOURS: " + model)
    //println(s"Times:  ${tann1} vs  ${tann2}")
    //println("WIDENINGS: " + program.locations.filter(program.isJoinNode).map(_.name).mkString(", "))
    println("EQUALS: " + program.locations.filter(comp(_) contains 0).map(_.name).mkString(", "))
    println("BETTER SUM: " + program.locations.filter(comp(_) contains -1).map(_.name).mkString(", "))
    println("BETTER OTHER: " + program.locations.filter(comp(_) contains 1).map(_.name).mkString(", "))
    println("UNCOMPARABLES: " + program.locations.filter(comp(_).isEmpty).map(_.name).mkString(", "))
  }

  var badPrograms = Seq[LTS]()

  // This analyzes all models (does not terminate for descending2 with

  for (model <- ltss) {
    try {
      fastModelAnalyze(model)
    } catch {
      case e: IllegalArgumentException =>
        badPrograms +:= model
    }
  }

  // This is if we want to analyze a specificic model
  // fastModelAnalyze(new File(resources.resolve("terminate.fst")))

  println("\nFinal results:")
  println("---------------")
  println(s"Number of programs: $totalPrograms")
  println(s"""Bad programs: ${badPrograms.mkString("\n")}""")
  println(s"Total equals: $totalEquals")
  println(s"Total best sum: $totalBestSum")
  println(s"Total best other: $totalBestOther")
  println(s"Total uncomparables: $totalUncomparable")
}
