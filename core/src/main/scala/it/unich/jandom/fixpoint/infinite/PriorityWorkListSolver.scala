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

import it.unich.jandom.fixpoint._
import it.unich.jandom.utils.PMaps._

/**
 * It solves a finite equation system by using a priority work-list method. It starts computing from the `start` assignment,
 * using the box operators in `boxes` and generating priorites for new unknowns with the `priorities` factory.
 * It determines at the least the unknowns specified in `wanted`.
 * $boxsolution
 * $termination
 */
class PriorityWorkListSolver[EQS <: EquationSystem](val eqs: EQS) extends LocalFixpointSolver[EQS] with FixpointSolverHelper[EQS] {
  
  /**
   * A parameter for the solver: a factory which return priorities for new unknowns. It defaults to a factory
   * which generates progressive descendant priorities.
   */
  val priorities = Parameter[UFactory[eqs.Unknown, Int]](new PriorityWorkListSolver.DynamicPriority[eqs.Unknown])

  /**
   * A parameter for the solver: the unknowns we want to have in our partial assignment
   */

  type Parameters = start.type +: boxes.type +: wanted.type +: priorities.type +: PNil

  def apply(params: Parameters): eqs.PartialAssignment = {
    implicit val listener = params(this.listener)
    val start = params(this.start)
    val boxes = params(this.boxes)
    val wanted = params(this.wanted)
    val priorities = params(this.priorities)

    val infl = new collection.mutable.HashMap[eqs.Unknown, collection.mutable.Set[eqs.Unknown]] with collection.mutable.MultiMap[eqs.Unknown, eqs.Unknown]
    var workList = collection.mutable.PriorityQueue.empty[eqs.Unknown](Ordering.by(priorities))
    workList ++= wanted

    class TrackAccess(rho: collection.mutable.Map[eqs.Unknown, eqs.Value], target: eqs.Unknown) extends eqs.Assignment {
      def apply(x: eqs.Unknown) = {
        if (!rho.isDefinedAt(x)) {
          rho += (x -> start(x))
          workList += x
        }
        infl.addBinding(x, target)
        rho(x)
      }
    }

    val current = initmap(start, wanted)
    while (!workList.isEmpty) {
      val x = workList.dequeue()
      val newval = evaluate(new TrackAccess(current, x), x, boxes(x))
      if (newval != current(x)) {
        current(x) = newval
        workList ++= infl(x)
        println(infl)
      }
    }
    current
  }
}

object PriorityWorkListSolver {
  
  /**
   * This class is a factory of priorities for unknowns, to be used together with the `PriorityWorkListSolver`. This
   * is the standard factory which is used if no priorities are specified.
   */
  class DynamicPriority[U] extends UFactory[U, Int] {
    var current = 0
    def apply(x: U) = {
      current -= 1
      current
    }
  }
}