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
 * This solver solver a finite equation system with a work-list based method.
 * @param eqs the equation system to solve
 */
final class WorkListSolver[EQS <: FiniteEquationSystem](val eqs: EQS) extends FixpointSolver[EQS] {

  def apply(start: eqs.Assignment, boxes: eqs.Unknown => eqs.Box): eqs.Assignment = {
    val current: collection.mutable.HashMap[eqs.Unknown, eqs.Value] =
       (for ( x <- eqs.unknowns) yield (x -> start(x))) (collection.breakOut)
    var workList = scala.collection.mutable.Queue[eqs.Unknown](eqs.unknowns: _*)
    while (! workList.isEmpty) {
      val x = workList.dequeue()
      val newval = boxes(x)(current(x), eqs(current)(x))
      if (newval != current(x)) {
        current(x) = newval
        workList.enqueue(eqs.infl(x): _*)
      }
    }
    current
  }
  val name = "WorkList"
}

object WorkListSolver {
  /**
   * Returns a solver for an equation system.
   * @param eqs the equation system to solve.
   */
  def apply(eqs: FiniteEquationSystem) = new WorkListSolver[eqs.type](eqs)
}
