/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.fixpoint

import it.unich.jandom.utils.PMaps._

/**
 * This trait contains some auxiliary methods which may be used by fixpoint solvers.
 */
protected trait FixpointSolverHelper[EQS <: EquationSystem] {
  this: FixpointSolver[EQS] =>

  /**
   * This helper method may be used by all fixpoint solvers when evaluating an expression. It returns the new value for the
   * unknown `x` computed by the assignment `rho` using `box`. It also calls all relevant listeners.
   * @param rho the current assignment
   * @param x the unknown we want to evaluate
   * @param box the box to use for composing the new value with the old one
   */
  @inline def evaluate(rho: eqs.Assignment, x: eqs.Unknown, box: Box[eqs.Value])(implicit listener: FixpointSolverListener): eqs.Value = {
    val newval = box(rho(x), eqs(rho)(x))
    listener.evaluated(rho, x, newval)
    newval
  }

  /**
   * This helper method builds a mutable hash for all the unknowns in wanted
   * @param start the assignment used to initialize the map
   * @param wanted the unknowns to fill the map with
   */
  @inline def initmap(start: eqs.Assignment, wanted: Iterable[eqs.Unknown]) (implicit listener: FixpointSolverListener): collection.mutable.Map[eqs.Unknown, eqs.Value] = {
    val rho = collection.mutable.HashMap.empty[eqs.Unknown, eqs.Value]
    rho ++= wanted map { u => u -> start(u) }
    listener.initialized(rho: eqs.Assignment)
    rho
  }
}
