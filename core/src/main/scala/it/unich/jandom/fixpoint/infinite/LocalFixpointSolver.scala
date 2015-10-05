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

import scala.language.higherKinds

import it.unich.jandom.fixpoint._

/**
 * A LocalFixpointSolver returns a partial solution of a possibly infinite equation system. Every
 * solver needs a parameter `wanted` which is a collection of unknowns we want to have in the result, and it
 * tries to solve the smallest set of unknowns in order to return a valid partial solution. A local
 * fixpoint solver should return an iterable function.
 */
trait LocalFixpointSolver extends FixpointSolver {
  
  /**
   * All pararameters for local solver inherit from this base class.
   */
  abstract protected class LocalBaseParams[U, V] extends BaseParams[U,V]{
    /**
     * The set of unknowns we want in the resulting assignment.
     */
    val wanted: Iterable[U]
  }
  
  type Params[U,V] <: LocalBaseParams[U,V]

  /**
   * The solve methods takes an equations system and parameters. If it terminates, the result is a
   * partial solution of the equation system.
   * @param eqs the equation system to solve
   * @param params the parameters to apply for solving the equation system
   */  
  def solve[U, V](eqs: EQS[U, V], params: Params[U, V]): PartialAssignment[U, V]
}
