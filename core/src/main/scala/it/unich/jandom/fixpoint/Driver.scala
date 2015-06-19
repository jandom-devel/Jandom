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

import it.unich.jandom.fixpoint._
import it.unich.jandom.utils.PMaps._

/**
 * The driver class has two aims: (1) it makes  easier to use fixpoint solvers when the parameters for
 * the algorithms are obtained by an user interface, and therefore may not be statically known; (2) combines
 * together different classes to implement standard behaviors. A driver is still a fixpoint solver,
 * but it does not implement any algorithm and just delegates to other fixpoint solvers by passing them suitable
 * parameters.
 */
abstract class Driver extends FixpointSolver {
  /**
   * This exception is thrown when the parameters provided to the `Driver` are not correct.
   */
  class DriverBadParameters(msg: String) extends Exception(msg)
}

/**
 * The driver companion object collects all parameters which may be passed to drivers.
 */
object Driver {

  // Parameters for the solver
  val solver = Parameter[Solver.Solver](Solver.WorkListSolver)
  val widening = Parameter[Widenings.Widening](Widenings.Default)
  val narrowing = Parameter[Narrowings.Narrowing](Narrowings.Default)
  val update = Parameter[Updates.Update](Updates.DefaultUpdate)
  val boxlocation = Parameter[BoxLocation.BoxLocation](BoxLocation.Loop)
  val boxscope = Parameter[BoxScope.Value](BoxScope.Standard)
  val boxstrategy = Parameter[BoxStrategy.Value](BoxStrategy.TwoPhases)
  val restartstrategy = Parameter[Boolean](false)
  val listener = Parameter[FixpointSolverListener[Any, Any]](FixpointSolverListener.EmptyListener)

  /**
   * An enumeration with the solvers supported by this driver.
   */
  object Solver extends Enumeration {
    val KleeneSolver = Value
    val RoundRobinSolver = Value
    val PriorityWorkListSolver = Value
    val WorkListSolver = Value
    val HierarchicalOrderingSolver = Value
    type Solver = Value
  }

  object Widenings {
    sealed abstract class Widening
    /**
     * Default widening.
     */
    object Default extends Widening

    /**
     * Union used as a widening.
     */
    object Union extends Widening

    /**
     * No widening at all.
     */
    object None extends Widening

    /**
     * Delayed narrowing. `first` is used for `delay` iteration, then `next` is used.
     */
    final case class Delayed(first: Widening, delay: Int, next: Widening) extends Widening
  }

  object Narrowings {
    sealed abstract class Narrowing

    /**
     * Default narrowing.
     */
    object Default extends Narrowing

    /**
     * A narrowing which immediatly stop the descending chain.
     */
    object Stop extends Narrowing

    /**
     * Intersection used as narrowing.
     */
    object Intersection extends Narrowing

    /**
     * No narrowing at all.
     */
    object None extends Narrowing

    /**
     * Delayed narrowing. `first` is used for `delay` iteration, then `next` is used.
     */
    final case class Delayed(first: Narrowing, delay: Int, next: Narrowing) extends Narrowing
  }

  object Updates {
    sealed abstract class Update
    /**
     * Standard mixed box obtained combining standard widening and narrowing.
     */
    object DefaultUpdate extends Update

    /**
     * A mixed box obtained combining the specified widening and narrowing.
     */
    final case class Combine(widening: Widenings.Widening, narrowing: Narrowings.Narrowing) extends Update
  }

  object BoxStrategy extends Enumeration {
    type BoxStrategy = Value
    /**
     * Only apply widening.
     */
    val OnlyWidening = Value

    /**
     * Standard two pass widening/narrowing iteration.
     */
    val TwoPhases = Value

    /**
     * Single pass with a mixed box.
     */
    val Mixed = Value
  }

  object BoxScope extends Enumeration {
    type BoxScope = Value
    /**
     * Use standard widening.
     */
    val Standard = Value
    /**
     * Use localized widening.
     */
    val Localized = Value
  }

  object BoxLocation extends Enumeration {
    type BoxLocation = Value
    /**
     * Put widening/narrowing points nowhere
     */
    val None = Value
    /**
     * Put widening/narrowing points at each unknown.
     */
    val All = Value
    /**
     * Put widening/narrowing points at each loop head.
     */
    val Loop = Value
  }
}
