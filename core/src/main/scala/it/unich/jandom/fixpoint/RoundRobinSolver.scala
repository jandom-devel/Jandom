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

import it.unich.jandom.utils.PMaps._

/**
 * This solver solves a finite equation system with the round robin method.
 * @param eqs the equation system to solve
 */
final class RoundRobinSolver[EQS <: FiniteEquationSystem](val eqs: EQS) extends FixpointSolver[EQS] {
  type Parameters = startParam.type +: boxesParam.type +: PNil

  def apply(params: Parameters): eqs.Assignment = {    
    val start = params(startParam)
    val boxes = params(boxesParam)
    
    val current: collection.mutable.HashMap[eqs.Unknown, eqs.Value] =
       (for ( x <- eqs.unknowns) yield (x -> start(x))) (collection.breakOut)
    var dirty = true
    while (dirty) {
      dirty = false
      for (x <- eqs.unknowns) {
        val newval = boxes(x)(current(x), eqs(current)(x))
        if (newval != current(x)) {
          current(x) = newval
          dirty = true
        }
      }
    }
    current
  }
  val name = "RoundRobin"
}

object RoundRobinSolver {
  /**
   * Returns a solver for an equation system.
   * @param eqs the equation system to solve.
   */
  def apply(eqs: FiniteEquationSystem) = new RoundRobinSolver[eqs.type](eqs)
}
