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
 * A solver for finite equation systems based on iterative strategies.
 * @param eqs the equation system to solve
 */
abstract class IterativeStrategySolver[EQS <: FiniteEquationSystem](val eqs: EQS) extends FixpointSolver[EQS] {

  /**
   * The iterative strategy to use for solving the equation system.
   */
  val strategy: IterativeStrategy[eqs.Unknown]

  def apply(start: eqs.Assignment, boxes: eqs.Unknown => eqs.Box): eqs.Assignment = {
    import IterativeStrategy._

    val current: collection.mutable.HashMap[eqs.Unknown, eqs.Value] = (for (x <- eqs.unknowns) yield (x -> start(x)))(collection.breakOut)
    val stack = collection.mutable.Stack.empty[Int]
    val stackdirty =  collection.mutable.Stack.empty[Boolean]

    var dirty = false
    var i = 0

    while (i < strategy.length) {
      strategy(i) match {
        case Left =>
          stack.push(i + 1)
          stackdirty.push(dirty)
          dirty = false
          i += 1
        case El(x) =>
          val newval = boxes(x)(current(x), eqs(current)(x))
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

  val name = "Strategy based"
}

object IterativeStrategySolver {
  /**
   * Returns a solver for an equation system with a given strategy.
   * @param eqs the equation system to solve.
   * @param a_strategy the iterative strategy to use.
   */
  def apply(eqs: FiniteEquationSystem)(a_strategy: IterativeStrategy[eqs.Unknown]) = new IterativeStrategySolver[eqs.type](eqs) { val strategy = a_strategy }
}
