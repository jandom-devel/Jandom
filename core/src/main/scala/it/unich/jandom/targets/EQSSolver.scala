/**
 * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.targets

import it.unich.jandom.targets.parameters.NarrowingSpecs._
import it.unich.jandom.targets.parameters.WideningNarrowingLocation
import it.unich.jandom.targets.parameters.WideningSpecs._
import it.unich.scalafix._
import it.unich.scalafix.FixpointSolver._
import it.unich.scalafix.FixpointSolverListener.EmptyListener
import it.unich.scalafix.finite.FiniteFixpointSolver
import it.unich.scalafix.finite.FiniteFixpointSolver._
import it.unich.scalafix.finite.GraphEquationSystem
import it.unich.jandom.targets.parameters.IterationStrategy
import it.unich.jandom.domains.AbstractDomain

/**
 * This is a solver for equations systems which takes a Parameters object in order to drive
 * the analysis.
 */

object EQSSolver {
  def apply[Tgt <: Target[Tgt]](tgt: Tgt)(dom: tgt.DomainBase)
        (params: Parameters[Tgt], listener: FixpointSolverListener[tgt.ProgramPoint, dom.Property] = FixpointSolverListener.EmptyListener)
        : Assignment[tgt.ProgramPoint, dom.Property] = {
    implicit val scalafixDomain = dom.ScalaFixDomain

    if (params.wideningLocation != params.narrowingLocation)
      throw new IllegalArgumentException("widening and narrowing locations shoule be the same");

    val widening = DefaultWidening.get(dom)
    val narrowing = DefaultNarrowing.get(dom)

    val boxlocation = params.wideningLocation match {
      case WideningNarrowingLocation.None => BoxLocation.None
      case WideningNarrowingLocation.All => BoxLocation.All
      case WideningNarrowingLocation.Loop => BoxLocation.Loop
    }

    val solver = params.iterationStrategy match {
      case IterationStrategy.Kleene => Solver.KleeneSolver
      case IterationStrategy.Worklist => Solver.WorkListSolver
    }

    val eqs = tgt.toEQS(dom)
    val eqsParams = FiniteFixpointSolver.CC77[tgt.ProgramPoint, dom.Property](solver, widening, narrowing).copy(
        boxlocation = boxlocation, listener = listener
    )

    eqs match {
      case eqs: GraphEquationSystem[tgt.ProgramPoint, dom.Property, _] => FiniteFixpointSolver(eqs, eqsParams)
      case _ => ???
    }
  }
}
