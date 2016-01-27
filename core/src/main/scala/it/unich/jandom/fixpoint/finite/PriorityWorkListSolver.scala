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
 * A fixpoint solver based on priority worklists.
 */
object PriorityWorkListSolver extends FiniteFixpointSolver {
  /**
   * Parameters needed for the priority worklist solver
   * @param start the initial assignment.
   * @param ordering an ordering which specified priorities between unknowns.
   * @param restart at each iteration this function it is applied to the new and old values. If it returns true, the analysis of bigger unknown is
   * restarted from the initial value.
   * @param listener the listener whose callbacks are invoked for debugging and tracing.
   */
  case class Params[U, V](start: Assignment[U,V], ordering: Ordering[U], restart: (V,V) => Boolean = { (x: V, y: V) => false }, listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener)  extends BaseParams[U,V]
  
  type EQS[U,V] = FiniteEquationSystem[U,V]

  def solve[U, V](eqs: FiniteEquationSystem[U, V], params: Params[U,V]) = {
    import params._
    
    val current = (collection.mutable.HashMap.empty[U, V]).withDefault(start)
    listener.initialized(current)
    var workList = scala.collection.mutable.PriorityQueue.empty[U](ordering)
    workList ++= eqs.unknowns
    while (!workList.isEmpty) {
      val x = workList.dequeue()
      val newval = eqs.body(current)(x)
      listener.evaluated(current, x, newval)
      val oldval = current(x)
      if (restart(newval, oldval))
        for (y <- eqs.unknowns; if ordering.gt(y,x))
          current(y) = start(y)
      if (newval != oldval) {
        current(x) = newval
        workList ++= eqs.infl.image(x)
      }

    }
    current
  }
  
    /**
   * A convenience method for calling the solver
   */
  def apply[U,V](eqs: EQS[U, V], start: Assignment[U, V], ordering: Ordering[U], restart: (V,V) => Boolean = { (x: V, y: V) => false }, listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) = 
    solve(eqs, Params(start, ordering, restart, listener))
}
