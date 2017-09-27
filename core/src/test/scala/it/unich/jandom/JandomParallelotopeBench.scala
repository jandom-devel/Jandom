/**
  * Copyright 2016, 2017 Jandom Team
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
import it.unich.jandom.targets.parameters.NarrowingSpecs._
import it.unich.jandom.targets.parameters.WideningSpecs._
import parma_polyhedra_library.C_Polyhedron

/**
  * Example program using ''Jandom'' to analyze the Alice benchmarks and
  * compare the results with different parameters. At the moment, it compares
  * the result of separately computing using box and parallelotope with respect
  * to using their product.
  */
object JandomParallelotopeBench extends App with FASTLoader {

  var totalEquals = 0
  var totalBestReduced = 0
  var totalBestSeparate = 0
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

    val params1 = new targets.Parameters[LTS] {
      val domain = new ProductDomain(BoxRationalDomain(), ParallelotopeRationalDomain(-1))
    }
    params1.widening = DelayedWidening(DefaultWidening, 1)
    params1.narrowing = DelayedNarrowing(DefaultNarrowing, 1)
    //params1.debugWriter = new java.io.PrintWriter(System.out)
    program.analyze(params1) // warmup JVM
    //params1.debugWriter.flush()

    val t1 = System.currentTimeMillis
    val productAnn = program.analyze(params1)
    val tann1  = System.currentTimeMillis - t1

    val params2 = new targets.Parameters[LTS] {
      val domain = BoxRationalDomain()
    }
    params2.widening = DelayedWidening(DefaultWidening, 1)
    params2.narrowing = DelayedNarrowing(DefaultNarrowing, 1)
    // params2.debugWriter = new java.io.PrintWriter(System.out)
    program.analyze(params2) // warmup JVMunchecked
    //params2.debugWriter.flush()

    val t2 = System.currentTimeMillis
    val boxAnn = program.analyze(params2)
    val tann2 = System.currentTimeMillis - t2

    val params3 = new targets.Parameters[LTS] {
      val domain = ParallelotopeRationalDomain(1)
    }
    params3.widening = DelayedWidening(DefaultWidening, 1)
    params3.narrowing = DelayedNarrowing(DefaultNarrowing, 1)
    //params6Bis.debugWriter = new java.io.PrintWriter(System.out)
    program.analyze(params2) // warmup JVM
    //params6Bis.debugWriter.flush()

    val t3 = System.currentTimeMillis
    val parAnn = program.analyze(params3)
    val tann3 = System.currentTimeMillis - t3

    val cprod = productAnn mapValues { p => CStoPolyehdra(p.p1.dimension, p.p1.constraints) intersection CStoPolyehdra(p.p2.dimension, p.p2.constraints) }
    val cbox = boxAnn mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }
    val cpar = parAnn mapValues { p => CStoPolyehdra(p.dimension, p.constraints) }

    print("Box    : ")
    println(mkString(program, cbox))
    print("Par    : ")
    println(mkString(program, cpar))
    print("Product: ")
    println(mkString(program, cprod))

    val comp = cprod map { case (loc, v) => loc -> v.tryCompareTo(cbox(loc) intersection cpar(loc)) }

    println(s"TIME REQUIRED: product: $tann1 box: $tann2 ptope: $tann3")
    println("COUNT EQUALS: " + comp.count(_._2 contains 0))
    println("COUNT BETTER REDUCED PRODUCT: " + comp.count(_._2 contains -1))
    println("COUNT BETTER SEPARATE ANALYSIS: " + comp.count(_._2 contains 1))
    println("COUNT UNCOMPARABLES: " + comp.count(_._2.isEmpty))
    totalEquals += comp.count(_._2 contains 0)
    totalBestReduced += comp.count(_._2 contains -1)
    totalBestSeparate += comp.count(_._2 contains 1)
    totalUncomparable += comp.count(_._2.isEmpty)

    println("EQUALS: " + program.locations.filter(comp(_) contains 0).map(_.name).mkString(", "))
    println("BETTER PRO: " + program.locations.filter(comp(_) contains -1).map(_.name).mkString(", "))
    println("BETTER INT: " + program.locations.filter(comp(_) contains 1).map(_.name).mkString(", "))
    println("UNCOMPARABLES: " + program.locations.filter(comp(_).isEmpty).map(_.name).mkString(", "))
  }

  val program: Option[String] = None
  var badPrograms = Seq[LTS]()

  if (program.isEmpty) {
    for (model <- ltss) {
      try {
        fastModelAnalyze(model)
      } catch {
        case e: IllegalArgumentException =>
          badPrograms +:= model
      }
    }
  } else {
    val model = ltss find (lts => program contains lts.name)
    model foreach fastModelAnalyze
  }

  println("\nFinal results:")
  println("---------------")
  println(s"Number of programs: $totalPrograms")
  println(s"Bad programs: ${badPrograms.mkString("\n")}")
  println(s"Total equals: $totalEquals")
  println(s"Total best reduced product: $totalBestReduced")
  println(s"Total best separate analysis: $totalBestSeparate")
  println(s"Total uncomparables: $totalUncomparable")
}
