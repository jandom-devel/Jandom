/**
  * Copyright 2017 Gianluca Amato <gianluca.amato@unich.it>
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
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.targets.lts.Location
import it.unich.scalafix.FixpointSolver._
import it.unich.scalafix.finite.FiniteFixpointSolver
import it.unich.scalafix.lattice.Domain
import it.unich.scalafix.{EquationSystemTracer, FixpointSolverTracer}

/**
  * An example application which analyze a FAST model which Scalafix and print a detailed
  * trace of the analysis.
  */
object FASTTracer extends App with FASTLoader {
  val dom = BoxDoubleDomain()
  implicit val scalaFixDomain: Domain[dom.Property] = dom.ScalaFixDomain

  val widening = dom.defaultWidening
  val narrowing = dom.defaultNarrowing
  val cc77 = FiniteFixpointSolver.CC77[Location, dom.Property](Solver.WorkListSolver, widening, narrowing)
  val cc77Tracing = cc77.copy(tracer = FixpointSolverTracer.debug)
  val lts = ltss.find(_.name == "amato_scozzari_sas13_hh -- as-hh.fst").get
  val eqs = lts.toEquationSystem(dom).withTracer(EquationSystemTracer.debug)
  FiniteFixpointSolver(eqs, cc77Tracing)
}
