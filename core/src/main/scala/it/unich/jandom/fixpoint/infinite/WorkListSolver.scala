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
 * It solves a finite equation system by using a work-list method. It starts computing from the `start` assignment,
 * using the box operators in `boxes`. It determines at the least the unknowns specified in `wanted`.
 * $boxsolution
 * $termination
 */
class WorkListSolver[EQS <: EquationSystem](val eqs: EQS) extends LocalFixpointSolver[EQS] with FixpointSolverHelper[EQS] {

  type Parameters = start.type +: boxes.type +: wanted.type +: PNil

  def apply(params: Parameters): eqs.PartialAssignment = {
    implicit val listener = params(this.listener)
    val start = params(this.start)
    val boxes = params(this.boxes)
    val wanted = params(this.wanted)

    var infl = new collection.mutable.HashMap[eqs.Unknown, collection.mutable.Set[eqs.Unknown]] with collection.mutable.MultiMap[eqs.Unknown, eqs.Unknown]
    var workList = collection.mutable.Queue.empty[eqs.Unknown]
    workList ++= wanted

    /*
     * This class, given a map `rho` and a `target` unknown, tracks all accesses to the map `rho` and updates the
     * `infl` map accordingly.
     */
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
