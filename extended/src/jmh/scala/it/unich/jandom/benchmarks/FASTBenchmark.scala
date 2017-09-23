/**
  * Copyright 2015, 2016 Gianluca Amato <gianluca.amato@unich.it>
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
import it.unich.jandom.domains.numerical.{BoxDoubleDomain, NumericalDomain}
import it.unich.jandom.targets.Parameters
import it.unich.jandom.targets.lts.{LTS, Location}
import it.unich.scalafix.FixpointSolver._
import it.unich.scalafix.finite.FiniteFixpointSolver
import it.unich.scalafix.lattice.Domain
import org.openjdk.jmh.annotations._

/**
  * This is a program which analyzes the Alice benchmarks with different settings and compares the execution time.
  */
@State(Scope.Thread)
@Warmup(iterations = 5)
class FASTBenchmark extends FASTLoader {

  val dom = BoxDoubleDomain()

  private implicit val scalafixDomain: Domain[dom.Property] = dom.ScalaFixDomain
  private val wideningBox = { (x: dom.Property, y: dom.Property) => x widening y }
  private val narrowingBox = { (x: dom.Property, y: dom.Property) => x narrowing y }
  private val CC77 = FiniteFixpointSolver.CC77[Location, dom.Property](Solver.WorkListSolver, wideningBox, narrowingBox)

  @Benchmark
  def timeLTS() {
    val params = new Parameters[LTS] {
      val domain: NumericalDomain = dom
    }
    for (lts <- ltss) lts.analyze(params)
  }

  @Benchmark
  def timeEQSKleene() {
    for (lts <- ltss) {
      val eqs = lts.toEQS(dom)
      FiniteFixpointSolver(eqs, CC77.copy(solver = Solver.KleeneSolver))
    }
  }

  @Benchmark
  def timeEQSRoundRobin() {
    for (lts <- ltss) {
      val eqs = lts.toEQS(dom)
      FiniteFixpointSolver(eqs, CC77.copy(solver = Solver.RoundRobinSolver))
    }
  }

  @Benchmark
  def timeEQSDefault() {
    for (lts <- ltss) {
      val eqs = lts.toEQS(dom)
      FiniteFixpointSolver(eqs, CC77)
    }
  }

  @Benchmark
  def timeEQSLocalized() {
    for (lts <- ltss) {
      val eqs = lts.toEQS(dom)
      FiniteFixpointSolver(eqs, CC77.copy(boxscope = BoxScope.Localized))
    }
  }

  @Benchmark
  def timeEQSMixedLocalized() {
    for (lts <- ltss) {
      val eqs = lts.toEQS(dom)
      FiniteFixpointSolver(eqs, CC77.copy(boxscope = BoxScope.Localized, boxstrategy = BoxStrategy.Warrowing))
    }
  }
}
