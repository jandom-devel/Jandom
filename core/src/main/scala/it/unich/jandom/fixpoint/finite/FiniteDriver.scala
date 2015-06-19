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
import it.unich.jandom.fixpoint.Driver._
import scala.annotation.implicitNotFound
import it.unich.jandom.utils.PMaps._

/**
 * This driver is an interface for solvers of finite equation systems.
 */
object FiniteDriver extends Driver {
  /**
   * This driver is used to call one of the solvers for finite equation systems. Allowed parameters are:
   * @param start the start assignment from where to start the computation
   * @param ordering the ordering on unknowns to use. When solver is `HierarchicalOrderingSolver`, the order should be
   * a `HierarchicalOrdering`.
   * @param restart at each iteration it is applied to the new and old values. If it returns true, the analysis of
   * lower priority unknowns is restarted.
   * @param p other parameters provided trough a PMap
   */
  def apply[U, V](eqs: FiniteEquationSystem[U, V], start: U => V, ordering: Option[Ordering[U]], restart: (V,V) => Boolean = { (x: V, y: V) => false }, p: PNil) = {
    val listener = p(Driver.listener)
    p(solver) match {
      case Solver.RoundRobinSolver => RoundRobinSolver(eqs,start,listener)
      case Solver.KleeneSolver => KleeneSolver(eqs,start,listener)
      case Solver.WorkListSolver => WorkListSolver(eqs,start,listener)
      case Solver.PriorityWorkListSolver => PriorityWorkListSolver(eqs, start, ordering.get, restart, listener)
      case Solver.HierarchicalOrderingSolver =>
        ordering.get match {
          case ho: HierarchicalOrdering[U] => HierarchicalOrderingSolver(eqs, start, ho,listener)
          case _ => throw new DriverBadParameters("Ordering bust me a hierarchical order for the hierachical ordering solver")
        }
    }
  }
}
