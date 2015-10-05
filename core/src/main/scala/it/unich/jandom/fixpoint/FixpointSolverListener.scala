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

import scala.annotation.elidable
import scala.annotation.elidable._

/**
 * A FixpointSolverListener implements some methods which are called by solvers when certain
 * events occurs. They may be used for debugging, tracing, etc...
 * @tparam U the type of unknowns supported by this listener
 * @tparam V the type of values for unknowns supported by this listener
 */
trait FixpointSolverListener[-U, -V] {
  /**
   * This method is called when an assignment is initialized with the start value.
   * @param rho the current assignment
   */
  @elidable(ASSERTION)
  def initialized[U1 <: U, V1 <: V](rho: U1 => V1)

  /**
   * This method is called when an unknown `u` is evaluated.
   * @param rho the current assignment
   * @param u the unknown which is evaluated
   * @param newval the result of the evaluation
   */
  @elidable(ASSERTION)
  def evaluated[U1 <: U, V1 <: V](rho: U1 => V1, u: U1, newval: V1)

  /**
   * This is called when the ascending phase begins in a two phase solver.
   * @param rho the assignment at the beginning of the ascending phase
   */
  @elidable(ASSERTION)
  def ascendingBegins[U1 <: U, V1 <: V](rho: U1 => V1)

  /**
   * This is called when the descending phase begins in a two phase solver.
   * @param rho the assignment at the beginning of the descending phase
   */
  @elidable(ASSERTION)
  def descendingBegins[U1 <: U, V1 <: V](rho: U1 => V1)
}

/**
 * This abstract class implements a listener for fixpoint solvers which does nothing.
 * May be sub-classed in order to override only the methods we are interested in.
 */
abstract class FixpointSolverListenerAdapter[-U, -V] extends FixpointSolverListener[U, V] {
  def evaluated[U1, V1](rho: U1 => V1, u: U1, newval: V1) {}
  def initialized[U1, V1](rho: U1 => V1) {}
  def ascendingBegins[U1, V1](rho: U1 => V1) {}
  def descendingBegins[U1, V1](rho: U1 => V1) {}
}

object FixpointSolverListener {

  /**
   * An empty listener which does nothing.
   */
  implicit object EmptyListener extends FixpointSolverListenerAdapter[Any, Any]

  /**
   * A listener which prints all the informations on the standard outputs.
   */
  object DebugListener extends FixpointSolverListener[Any, Any] {
    def evaluated[U1, V1](rho: U1 => V1, u: U1, newval: V1) { println(s"evaluated: ${u} oldvalue: ${rho(u)} newvalue ${newval}") }
    def initialized[U1, V1](rho: U1 => V1) { println("initialized with assignment ${rho}") }
    def ascendingBegins[U1, V1](rho: U1 => V1) { println("ascending chain begins with assignment ${rho}") }
    def descendingBegins[U1, V1](rho: U1 => V1) { println("descending chain begins with assignment ${rho}") }
  }

  /**
   * A listener which keeps track of performance measures.
   */
  class PerformanceListener extends FixpointSolverListenerAdapter[Any, Any] {

    private var numeval: Int = 0    

    /**
     * Number of evaluations of r.h.s. performed so far
     */
    def evaluations = numeval

    override def evaluated[U1, V1](rho: U1 => V1, u: U1, newval: V1) { numeval += 1 }
  }
}
