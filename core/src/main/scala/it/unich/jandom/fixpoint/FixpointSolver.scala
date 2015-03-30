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

/**
 * This is the common trait of all equation solvers.
 * @tparam EQS the type of equation systems supported by this solver
 */
trait FixpointSolver[EQS <: EquationSystem] {

  /**
   * The particular equation system this solver deals with.
   */
  val eqs: EQS

  /**
   * The solver algorithm.
   */
  def apply(start: eqs.Assignment, boxes: eqs.Unknown => eqs.Box): eqs.Assignment

  /**
   * The name of the solver.
   */
  val name: String
}
