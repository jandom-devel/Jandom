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

import scala.collection.mutable.Buffer

/**
 * This is the trait for a generic equation system.
 * @tparam U the type for the unknowns of this equation system.
 * @tparam V the type for the values assumed by the unknowns of this equation system.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait EquationSystem[U, V] {   
  /**
   * The equation system viewed as a transformer from assignments to assignments.
   */
  def body: (U => V) => (U => V)

  /**
   * An initial assignment which may be used to bootstrap the analysis. 
   */
  def initial: U => V
  
  /**
   * A variant of `body` which keeps track of the unknowns queried during the evaluation.
   */
  def bodyWithDependencies: (U => V) => U => (V, Iterable[U])

  /**
   * Add boxes to the equation system. 
   * @param boxes a partial function from unknowns to boxes.
   * @param boxesAreIdempotent if true boxes are assumed to be idempotent. In this case, some optimization.
   * is possible.
   */
  def withBoxes(boxes: PartialFunction[U, Box[V]], boxesAreIdempotent: Boolean): EquationSystem[U, V]
}

object EquationSystem {

  /**
   * An alias for the type returned by body
   */
  type Body[U, V] = (U => V) => (U => V)

  /**
   * An alias for the type returned by bodyWithDependencies
   */
  type BodyWithDependecies[U, V] = (U => V) => U => (V, Iterable[U])

  /**
   * Returns a new equation system from the given body.
   */
  def apply[U, V](body: Body[U, V], initial: U => V) = SimpleEquationSystem(body, initial)
  
  /**
   * This class implements an equation system when a body is provided.
   */
  case class SimpleEquationSystem[U, V](val body: Body[U, V], val initial: U => V) extends EquationSystem[U,V] {
    def bodyWithDependencies = buildBodyWithDependencies(body)
    def withBoxes(boxes: PartialFunction[U, Box[V]], boxesAreIdempotent: Boolean) =
      copy(body = addBoxesToBody(body, boxes))
    def withInitialAssignment(rho: U => V) =
      copy(initial = rho)
  }
  
  /**
   * This method takes a body and a partial box assignment and returns a new body where boxes have been
   * plugged inside. If `newbody = addBoxesToBody(body, boxes)` and `boxes` is defined on `x`,
   * then `newbody(rho)(x) = boxes(x) (  rho(x), body(rho)(x) )`.
   */
  def addBoxesToBody[U, V](body: Body[U, V], boxes: PartialFunction[U, Box[V]]): Body[U,V] = {
    (rho: U => V) =>
      (x: U) => {
        if (boxes.isDefinedAt(x)) boxes(x)(rho(x), body(rho)(x)) else body(rho)(x)
      }
  }

  /**
   * This methods takes a body and returns a new body which keeps tracks of queried unknowns.
   */
  def buildBodyWithDependencies[U, V](body: Body[U,V]): BodyWithDependecies[U,V] = {
    (rho: U => V) =>
      (x: U) => {
        val queried = Buffer.empty[U]
        val trackrho = { y: U =>
          queried.append(y)
          rho(y)
        }
        val newval = body(trackrho)(x)
        (newval, queried.toSeq)
      }
  }
}
