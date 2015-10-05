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

import it.unich.jandom.fixpoint._

/**
 * A fixpoint solver based on a worklist.
 */
object WorkListSolver extends FiniteFixpointSolver {
  /**
   * Parameters needed for the round robin solver
   * @param start the initial assignment.
   * @param listener the listener whose callbacks are invoked for debugging and tracing.
   */
  case class Params[U, V](start: Assignment[U, V], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) extends BaseParams[U,V]

  type EQS[U, V] = FiniteEquationSystem[U, V]

  /**
   * It solves a finite equation system by using a worklist based method.
   * @param eqs the equation system to solve.
   * @param start the initial assignment.
   * @param litener the listener whose callbacks are called for debugging and tracing.
   */
  def solve[U, V](eqs: FiniteEquationSystem[U, V], params: Params[U,V]) = {
    import params._
    
    val current = (collection.mutable.HashMap.empty[U, V]).withDefault(start)
    listener.initialized(current)
    var workList = collection.mutable.Queue.empty[U]
    workList ++= eqs.unknowns
    while (!workList.isEmpty) {
      val x = workList.dequeue()
      val newval = eqs.body(current)(x)
      listener.evaluated(current, x, newval)
      if (newval != current(x)) {
        current(x) = newval
        // this might become expensive if worklist is big... however, blindly 
        // adding to the worklist all the elements in the influence set greatly
        // increases the number of iterations.
        for (y <- eqs.infl.image(x); if !(workList contains y)) workList += y
      }
    }
    current
  }

  def apply[U, V](eqs: FiniteEquationSystem[U, V], start: Assignment[U,V], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) =
    solve(eqs, Params(start, listener))
}
