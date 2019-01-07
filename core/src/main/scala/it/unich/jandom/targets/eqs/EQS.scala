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

package it.unich.jandom.targets.eqs

import it.unich.jandom.domains.AbstractDomain
import it.unich.jandom.targets.parameters.{IterationStrategy, WideningNarrowingLocation, WideningScope}
import it.unich.jandom.targets.{Annotation, Target}
import it.unich.scalafix.FixpointSolver.{BoxLocation, BoxScope, Solver}
import it.unich.scalafix.finite.{FiniteFixpointSolver, GraphEquationSystem}

/**
  * A target for equation systems.
  *
  * @tparam U the unknowns of the equation system
  * @tparam V the values of the equation system
  * @param eqs    a graph equation system with unknowns U and values V
  * @param lastPP the last program point, which is None by default
  * @return an EQS target build from the `eqs` GraphEquationSystem
  */
class EQS[U, V](eqs: GraphEquationSystem[U, V, _], val lastPP: Option[U] = None) extends Target[EQS[U, V]] {
  type ProgramPoint = U
  type DomainBase = AbstractDomain {type Property = V}

  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property] = {
    import params._
    implicit val scalafixDomain = params.domain.ScalaFixDomain

    if (params.wideningLocation != params.narrowingLocation)
      throw new IllegalArgumentException("widening and narrowing locations should be the same");

    val boxlocation = wideningLocation match {
      case WideningNarrowingLocation.None => BoxLocation.None
      case WideningNarrowingLocation.All => BoxLocation.All
      case WideningNarrowingLocation.Loop => BoxLocation.Loop
    }

    val solver = iterationStrategy match {
      case IterationStrategy.Kleene => Solver.KleeneSolver
      case IterationStrategy.Worklist => Solver.WorkListSolver
    }

    val boxscope = wideningScope match {
      case WideningScope.Output => BoxScope.Standard
      case WideningScope.Random => BoxScope.Localized
      case WideningScope.BackEdges =>
        throw new IllegalArgumentException("Widening on back edges not supported")
    }

    val eqsParams = FiniteFixpointSolver.CC77[U, domain.Property](solver, widening, narrowing).copy(
      boxlocation = boxlocation, boxscope = boxscope
    )

    val rho = FiniteFixpointSolver(eqs, eqsParams)
    val ann = getAnnotation[domain.Property]
    for (u <- eqs.unknowns) ann(u) = rho(u)
    ann
  }
}

/**
  * The companion object for the EQS class.
  */
object EQS {
  def apply[U, V](eqs: GraphEquationSystem[U, V, _], lastPP: Option[U] = None) = new EQS(eqs, lastPP)
}
