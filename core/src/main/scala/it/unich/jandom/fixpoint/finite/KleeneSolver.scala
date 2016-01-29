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
 * A solver based on Kleene iteration.
 */
object KleeneSolver extends FiniteFixpointSolver {
  /**
   * Parameters needed for the Kleene solver
   * @param start the initial assignment.
   * @param listener the listener whose callbacks are invoked for debugging and tracing.
   */
  case class Params[U, V](start: Assignment[U, V], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) extends BaseParams[U,V]

  type EQS[U, V] = FiniteEquationSystem[U, V]

  /**
   * It solves a finite equation system by applying Kleene iteration from the `start` assignment.
   * @param eqs the equation system to solve.
   */
  def solve[U, V](eqs: EQS[U, V], params: Params[U, V]) = {
    import params._

    var current = (collection.mutable.HashMap.empty[U, V]).withDefault(start)
    var next = collection.mutable.HashMap.empty[U, V]
    listener.initialized(current)
    var dirty = true
    while (dirty) {
      dirty = false
      for (x <- eqs.unknowns) {
        val newval = eqs.body(current)(x)
        listener.evaluated(current, x, newval)
        if (newval != current(x)) dirty = true
        next(x) = newval
      }
      current = next
    }
    current
  }

  /**
   * A convenience method for calling the solver
   */
  def apply[U, V](eqs: EQS[U, V], start: Assignment[U, V], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) =
    solve(eqs, Params(start, listener))
}