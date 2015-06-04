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
 * A FixpointSolverListener implements some methods which are called by solvers when certain
 * events occurs. They may be used for debugging, tracing, etc...
 */
trait FixpointSolverListener {
  /**
   * This method is called when an assignment is initialized with the start value
   * @tparam EQS the type of equation system 
   * @param rho the current assignment
   */
  def initialized[EQS <: EquationSystem](rho: EQS#Assignment)
  
  /**
   * This method is called when an unknown `u` is evaluated.
   * @tparam EQS the equation  
   * @param rho the current assignment
   * @param x the unknown which is evaluated
   * @param x the result of the evaluation
   */
  def evaluated[EQS <: EquationSystem](rho: EQS#Assignment, u: EQS#Unknown, newval: EQS#Value)
}

object FixpointSolverListener {
  /**
   * An empty listener which does nothing.
   */
  object EmptyListener extends FixpointSolverListener {
    def evaluated[EQS <: EquationSystem](rho: EQS#Assignment, u: EQS#Unknown, newval: EQS#Value) { }
    def initialized[EQS <: EquationSystem](rho: EQS#Assignment) { }
  }
}
