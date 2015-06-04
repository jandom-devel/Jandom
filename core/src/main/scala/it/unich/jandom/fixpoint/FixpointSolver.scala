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
 * This is the common trait of all fixpoint solvers for equation systems.
 * @tparam EQS the type of equation systems supported by this solver.
 * @define boxsolution If it terminates, it returns a box-solution of the set of equations.
 * @define termination It is guaranteed to terminate if each loop in the dependency graph of `eqs` has a node `u` such that
 * the box associated to `u` is a widening.
 */
abstract class FixpointSolver[EQS <: EquationSystem] {
  /**
   * The particular equation system this solver deals with.
   */
  val eqs: EQS

  /**
   * A parameter for the solver: a listener. It defaults to the empty listener. Almost
   * every fixpoint solver uses the `listener` parameter for tracing and debugging
   * purposes, although it is never explicitly mentioned in the documentation.
   */
  val listener = Parameter[FixpointSolverListener](FixpointSolverListener.EmptyListener)

  /**
   * A parameter for the solver: the starting assignment.
   */
  val start = Parameter[eqs.Assignment]

  /**
   * A parameter for the solver: an assignment of boxes to unknowns.
   */
  val boxes = Parameter[eqs.BoxAssignment]

  /**
   * The set of parameters required by this particular solver.
   */
  type Parameters <: PMap
  
  /**
   * The solver algorithm. If it terminates, it returns a solution of the equation system, whatever it means.
   * @param params the parameters for the algorithm.
   */
  def apply(params: Parameters): eqs.Assignment
}
