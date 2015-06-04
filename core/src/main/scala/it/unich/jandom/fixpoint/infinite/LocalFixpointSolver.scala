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

package it.unich.jandom.fixpoint.infinite

import it.unich.jandom.fixpoint._
import it.unich.jandom.utils.PMaps._

/**
 * A LocalFixpointSolver returns a partial solution of a possibly infinite equation system. It needs a 
 * parameter `wanted` which is a collection of unknowns we want to have in the result, and it
 * tries to solve the smallest set of unknowns in order to return a valid partial solution.
 */
abstract class LocalFixpointSolver[EQS <: EquationSystem] extends FixpointSolver[EQS] {
  /**
   * A parameter for the solver: the collection of required unknowns in the result.
   */
  val wanted = Parameter[Iterable[eqs.Unknown]] 
  
  /**
   * The solver algorithm. It returns a partial assignment, so that it may be queried for the
   * actual set of unknowns which have been computed. If it terminates, it returns a solution 
   * of the equation system, whatever it means.
   */
  def apply(params: Parameters): eqs.PartialAssignment
}