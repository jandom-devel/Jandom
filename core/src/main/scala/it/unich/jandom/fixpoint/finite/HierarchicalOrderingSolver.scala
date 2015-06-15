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
 * A solver whose strategy in based on a hierarchical ordering.
 */
object HierarchicalOrderingSolver extends FixpointSolver {
  /**
   * It solves a finite equation system by using a strategy encoded with a hierarchical ordering.
   * @param eqs the equations system to solve.
   * @param start the initial assignment.
   * @param ordering the hierarchical ordering which drives the analysis.
   * @param litener the listener whose callbacks are called for debugging and tracing.
   */
  def apply[U, V](eqs: FiniteEquationSystem[U, V], start: U => V, ordering: HierarchicalOrdering[U], listener: FixpointSolverListener[U, V] = FixpointSolverListener.EmptyListener) = {
    import HierarchicalOrdering._

    val current = (collection.mutable.HashMap.empty[U, V]).withDefault(start)
    listener.initialized(current)
    val stack = collection.mutable.Stack.empty[Int]
    val stackdirty = collection.mutable.Stack.empty[Boolean]

    var dirty = false
    var i = 0
    val sequence = ordering.toSeqWithParenthesis

    while (i < sequence.length) {
      sequence(i) match {
        case Left =>
          stack.push(i + 1)
          stackdirty.push(dirty)
          dirty = false
          i += 1
        case Val(x) =>
          val newval = eqs.body(current)(x)
          listener.evaluated(current, x, newval)
          if (newval != current(x)) {
            current(x) = newval
            dirty = true
          }
          i += 1
        case Right =>
          if (dirty) {
            i = stack.top
            dirty = false
          } else {
            stack.pop
            dirty = stackdirty.pop()
            i += 1
          }
      }
    }
    current
  }
}
