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

package it.unich.jandom.fixpoint.finite

import scala.language.higherKinds

import it.unich.jandom.fixpoint.Assignment
import it.unich.jandom.fixpoint.FixpointSolver

/**
 * A FiniteFixpointSolver only works with finite equation systems. 
 */
trait FiniteFixpointSolver extends FixpointSolver {  
  /**
   * This solver only works with finite equation systems.
   */
  type EQS[U,V] <: FiniteEquationSystem[U,V]
      
  /**
   * The solve methods takes a finite equations system and parameters. If it terminates, the result is a
   * solution of the equation set.
   * @param eqs the equation system to solve
   * @param params the parameters to apply for solving the equation system
   */  
  def solve[U,V](eqs: EQS[U,V], params: Params[U,V]): Assignment[U,V]
}
