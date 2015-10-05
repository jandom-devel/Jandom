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
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.fixpoint.infinite

import it.unich.jandom.fixpoint._
import it.unich.jandom.utils.IterableFunction

/**
 * A local fixpoint solver based on a worklist with priorities.
 */
object PriorityWorkListSolver extends LocalFixpointSolver {
  import EquationSystem._
  
  /**
   * Parameters needed for the local priority worklist solver
   * @param start the initial assignment.
   * @param listener the listener whose callbacks are invoked for debugging and tracing.
   */
  case class Params[U, V](start: U => V, wanted: Iterable[U], ordering: Ordering[U] = new DynamicPriority[U], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) extends LocalBaseParams[U,V]

  type EQS[U, V] = EquationSystem[U, V]
  
  /**
   * This is an dynamic ordering on unknowns: every time an unknown appears, it gets assigned a lower
   * priority of previous one (i.e., it come earlier in the ordering). This is the default ordering
   * for PriorityWorkListSolver when an explicit one is not provided.
   */
  class DynamicPriority[U] extends Ordering[U] {
    val map = collection.mutable.Map.empty[U,Int]
    var current = 0
    def compare(x: U, y: U) = {
      val xp = map getOrElseUpdate(x, { current -= 1; current })
      val yp = map getOrElseUpdate(y, { current -= 1; current })
      xp - yp
    }    
  }
  
  /**
   * It solves a finite equation system by using a worklist based method with priorities.
   * @param eqs the equation system to solve.
   * @param start the initial assignment.
   * @param wanted the collection of unknowns for which we want a solution.
   * @param ordering an ordering on all unknowns.
   * @param litener the listener whose callbacks are called for debugging and tracing.
   */
  def solve[U,V](eqs: EquationSystem[U,V], params: Params[U,V]) = {
    import params._
    
    val infl = new collection.mutable.HashMap[U, collection.mutable.Set[U]] with collection.mutable.MultiMap[U, U]
    var workList = collection.mutable.PriorityQueue.empty[U](ordering)
    workList ++= wanted

    val current = (collection.mutable.HashMap.empty[U, V]).withDefault(start)
    listener.initialized(current)
    while (!workList.isEmpty) {
      val x = workList.dequeue()
      val (newval, dependencies) = eqs.bodyWithDependencies(current)(x)
      listener.evaluated(current, x, newval)
      for (y <- dependencies) {
        if (!current.isDefinedAt(y)) {
          current(y) = start(y)
          workList += y
        }
        infl.addBinding(y, x)
      }
      if (newval != current(x)) {
        current(x) = newval
        workList ++= infl(x)
      }
    }
    current
  }
  
  /**
   * A convenience method for calling the solver
   */
  def apply[U,V](eqs: EquationSystem[U,V], start: U => V, wanted: Iterable[U], ordering: Ordering[U] = new DynamicPriority[U], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) = 
     solve(eqs,Params(start, wanted, ordering, listener))
}
