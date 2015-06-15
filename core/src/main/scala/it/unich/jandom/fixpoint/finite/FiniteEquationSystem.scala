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
import it.unich.jandom.utils.Relation

/**
 * This is the trait for an equation system with a finite set of unknowns
 * and static dependencies between them. When computing `apply(rho)(x)`,
 * the result may only depend on values of `rho(y)` for an `y` such that
 * `y infl x`.
 */
trait FiniteEquationSystem[U, V] extends EquationSystem[U, V] {
  /**
   * The collection of all unknowns.
   */
  val unknowns: Iterable[U]

  /**
   * The collection of all unknowns which may be considered the inputs to this equation system.
   */
  val inputUnknowns: Iterable[U]

  /**
   * The static relation between an unknown x and the unknowns y it influences. In any moment, the dependencies
   * returned by bodyWithDependecies(x) should be a subset of infl.reverse.image(x). The only exception
   * is when an unkwnown `x` influences `x` itself. In this case, `infl` may omit this dependence when
   * the r.h.s. for `x` is idempotent.
   */
  val infl: Relation[U, U]

  /**
   * Add boxes to the equation system.
   */
  def withBoxes(boxes: PartialFunction[U, Box[V]], boxesAreIdempotent: Boolean): FiniteEquationSystem[U, V]
}

object FiniteEquationSystem {
  import EquationSystem._

  /**
   * Returns a finite equation system given its constituents parts.
   */
  def apply[U, V](body: Body[U, V], unknowns: Iterable[U], inputUnknowns: Iterable[U], infl: Relation[U, U], initial: U => V) =
    new SimpleFiniteEquationSystem(body, unknowns, inputUnknowns, infl, initial)

  /**
   * A class defining a finite equation system given its constituents parts.
   */
  final case class SimpleFiniteEquationSystem[U, V](
      val body: Body[U, V],
      val unknowns: Iterable[U],
      val inputUnknowns: Iterable[U],
      val infl: Relation[U, U],
      val initial: U => V) extends FiniteEquationSystem[U, V] {

    def bodyWithDependencies = buildBodyWithDependencies(body)

    def withBoxes(boxes: PartialFunction[U, Box[V]], boxesAreIdempotent: Boolean) = copy(
      body = addBoxesToBody(body, boxes),
      infl = if (boxesAreIdempotent) infl else infl union Relation(unknowns.toSet, { (u: U) => Set(u) }))
  }
}
