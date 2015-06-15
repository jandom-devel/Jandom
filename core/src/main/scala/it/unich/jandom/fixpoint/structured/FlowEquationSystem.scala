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
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.fixpoint.structured

import it.unich.jandom.fixpoint._
import it.unich.jandom.fixpoint.finite._
import it.unich.jandom.utils.Relation
import it.unich.jandom.fixpoint.structured.LayeredEquationSystem.SimpleLayeredEquationSystem

/**
 * This is the trait for a layered equation system where each layer contain a single equation of the
 * type `x_i = f(x_j)`. In other words, each edge has one source and one target. This kind of
 * equation systems always arise for dataflow equations. Note, however, that it does not
 * seem to have any performance advantage w.r.t. the general case, and it is less expressive.
 * @tparam U the type for the unknowns of this equation system.
 * @tparam V the type for the values assumed by the unknowns of this equation system.
 * @tparam E the type of edges of this equation system
 */
trait FlowEquationSystem[U, V, E] extends LayeredEquationSystem[U, V, E] {

  /**
   * It return a function which, given an assignment and edge, returns the output value of
   * the edge.
   */
  def edgeAction: (U => V) => E => V

  /**
   * Maps each edge to its source unknown.
   */
  def source: E => U

  /**
   * Maps each edge to its target unknown.
   */
  def target: E => U

  /**
   * Maps each unknown to the collection of edges departing from it.
   */
  def outgoing: U => Iterable[E]

  /**
   * Maps each unknown to the collection of edges arriving on it.
   */
  def ingoing: U => Iterable[E]
}

object FlowEquationSystem {
  import EquationSystem._
  import LayeredEquationSystem._

  /**
   * An alias for the type of edgeAction.
   */
  type EdgeAction[U, V, E] = (U => V) => E => V

  /**
   * Returns an implementation of `FlowEquationSystem` from a subset of its constituents.
   */
  def apply[U, V, E](unknowns: Iterable[U], inputUnknowns: Iterable[U], edgeAction: EdgeAction[U, V, E],
    source: E => U, target: E => U, outgoing: U => Iterable[E], ingoing: U => Iterable[E], initial: U => V, combine: Box[V]) =
    SimpleFlowEquationSystem(unknowns, inputUnknowns, edgeAction, source, target, outgoing, ingoing, initial, combine)

  /**
   * An implementation of `FlowEquationSystem` from a subset of its constituents.
   */
  case class SimpleFlowEquationSystem[U, V, E](
      val unknowns: Iterable[U],
      val inputUnknowns: Iterable[U],
      val edgeAction: EdgeAction[U, V, E],
      val source: E => U,
      val target: E => U,
      val outgoing: U => Iterable[E],
      val ingoing: U => Iterable[E],
      val initial: U => V,
      val combine: Box[V]) extends FlowEquationSystem[U, V, E] {

    private lazy val edges: Set[E] = (for (u <- unknowns; e <- outgoing(u)) yield e)(collection.breakOut)

    lazy val sources = Relation(edges, { e: E => Set(source(e)) })

    lazy val targets = Relation(edges, { e: E => Set(target(e)) })

    def edgeBody = (e: E) => (rho: U => V) => (x: U) => edgeAction(rho)(e)

    def body = { (rho: U => V) =>
      (x: U) =>
        val contributions = for (e <- ingoing(x)) yield edgeAction(rho)(e)
        // if contribution is empty the unknown x has no right hand side... it seems
        // reasonable to return the old value.
        if (contributions.isEmpty) rho(x) else  contributions reduce { combine }
    }

    def bodyWithDependencies = { (rho: U => V) =>
      (x: U) =>
        val deps = for (e <- ingoing(x)) yield source(e)
        val res = body(rho)(x)
        (res, deps)
    }

    val infl: Relation[U, U] = new Relation[U, U] with Relation.AutomaticPartialOrdering[U, U] {
      original =>
      val domain = unknowns.toSet
      val codomain = domain
      def image(x: U) = (for (e <- outgoing(x)) yield target(e))(collection.breakOut)
      def apply(x: U, y: U) = outgoing(x).exists(target(_) == y)
      def graph = for (x <- domain; e <- ingoing(x)) yield (source(e), x)
      def isEmpty = unknowns.isEmpty
      def inverse = new Relation[U, U] with Relation.AutomaticPartialOrdering[U, U] {
        val domain = original.codomain
        val codomain = original.domain
        def image(x: U) = (for (e <- ingoing(x)) yield source(e))(collection.breakOut)
        def apply(x: U, y: U) = ingoing(x).exists(source(_) == y)
        def graph = original.graph map { case (x, y) => (y, x) }
        def isEmpty = unknowns.isEmpty
        def inverse = original
      }
    }

    def withInputAssignment(init: PartialFunction[U, V]) = FiniteEquationSystem(
      body = addInputAssignmentToBody(body, init, combine),
      unknowns = unknowns,
      inputUnknowns = inputUnknowns,
      infl = infl,
      initial = initial
    )

    def withBoxes(boxes: PartialFunction[U, Box[V]], boxesAreIdempotent: Boolean) = FiniteEquationSystem(
      body = addBoxesToBody(body, boxes),
      inputUnknowns = inputUnknowns,
      unknowns = unknowns,
      infl = if (boxesAreIdempotent) infl else infl union Relation(unknowns.toSet, { (u: U) => Set(u) }),
      initial = initial)

    def withLocalizedBoxes(boxes: PartialFunction[U, Box[V]], ordering: Ordering[U], boxesAreIdempotent: Boolean) = {
      if (boxesAreIdempotent) {
        val newEdgeAction: EdgeAction[U, V, E] = { (rho: U => V) =>
          e: E =>
            val x = target(e)
            if (boxes.isDefinedAt(x) && ordering.lteq(x, source(e))) {
              boxes(x)(rho(x), edgeAction(rho)(e))
            } else
              edgeAction(rho)(e)
        }
        copy(edgeAction = newEdgeAction)
      } else {
        val asLayered = LayeredEquationSystem(unknowns, inputUnknowns, edgeBody, sources, targets, initial, combine)
        asLayered.withLocalizedBoxes(boxes, ordering, boxesAreIdempotent)
      }
    }
  }
}
