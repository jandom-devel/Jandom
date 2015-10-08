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
import it.unich.jandom.fixpoint.lattice.Magma

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
  def body: Body[U, V]

  /**
   * A variant of `body` which keeps track of the unknowns queried during the evaluation.
   */
  def withDependencies: BodyWithDependecies[U, V]

  /**
   * Add boxes to the equation system.
   * @param boxes a partial function from unknowns to boxes.
   * @param boxesAreIdempotent if true boxes are assumed to be idempotent. In this case, some optimizations
   * are possible.
   */
  def withBoxes(boxes: BoxAssignment[U, V], boxesAreIdempotent: Boolean): EquationSystem[U, V]

  /**
   * Combine a base assignment with the equation system
   * @param init the assignment to add to the equation system
   */
  def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): EquationSystem[U,V]
}

object EquationSystem {
  /**
   * Returns a new equation system from the given body.
   */
  def apply[U, V](body: Body[U, V]) = SimpleEquationSystem(body)

  /**
   * This is a simple implementation of equation systems.
   */
  final case class SimpleEquationSystem[U, V](val body: Body[U, V]) extends EquationSystem[U, V] {
    val withDependencies = buildBodyWithDependencies(body)
    def withBoxes(boxes: BoxAssignment[U, V], boxesAreIdempotent: Boolean) =
      new SimpleEquationSystem(addBoxesToBody(body, boxes))
    def withBaseAssignment(init: PartialFunction[U, V])(implicit magma: Magma[V]): EquationSystem[U,V] =
      new SimpleEquationSystem(addBaseAssignmentToBody(body, init))
  }

  /**
   * This method builds a body with dependencies from a standard body.
   */
  def buildBodyWithDependencies[U, V](body: Body[U, V]) = {
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

  /**
   * This method takes a body and a partial box assignment and returns a new body where boxes have been
   * plugged inside. If `newbody = addBoxesToBody(body, boxes)` and `boxes` is defined on `x`,
   * then `newbody(rho)(x) = boxes(x) (  rho(x), body(rho)(x) )`.
   */
  def addBoxesToBody[U, V](body: Body[U, V], boxes: BoxAssignment[U, V]): Body[U, V] = {
    (rho: U => V) =>
      (x: U) => {
        if (boxes.isDefinedAt(x)) boxes(x)(rho(x), body(rho)(x)) else body(rho)(x)
      }
  }

  /**
   * This method takes a body, a partial assignment and a combine operation, and returns a new equation system
   * where the r.h.s. of each unknown is combined with init. In formulas, if `newbody` is the result of the method
   * and `init` is defined on 'x', then `newbody(rho)(x) = combine( init(x), body(rho)(x) )`.
   */
  def addBaseAssignmentToBody[U, V](body: Body[U, V], init: PartialFunction[U, V])(implicit magma: Magma[V]): Body[U, V] = {
    (rho: U => V) =>
      (x: U) => {
        if (init.isDefinedAt(x)) magma.op(init(x), body(rho)(x)) else body(rho)(x)
      }
  }

}
