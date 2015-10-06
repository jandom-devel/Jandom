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

/**
 * This is the trait for a finite equation system which is composed of several layers. Each layer is
 * an equation system by itself, and may be though of as an hyperedge with many sources and targets.
 * @tparam U the type for the unknowns of this equation system.
 * @tparam V the type for the values assumed by the unknowns of this equation system.
 * @tparam E the type of hyperedges/layer of this equation system
 */
trait LayeredEquationSystem[U, V, E] extends FiniteEquationSystem[U, V] {

  /**
   * It returns a function which associates a transformer of assignments to each body.
   */
  def edgeBody: E => (U => V) => (U => V)

  /**
   * A relation between edges and unknowns. The pair (e,u) is in the relation if u is a source
   * of e. Note that edgeBody(e) may use the unknown u only if (e,u) is in the relation sources.
   */
  def sources: Relation[E, U]

  /**
   * A relation between edges and unknowns. The pair (e,u) is in the relation if u is a target
   * of e. Note that edgeBody(e)(rho)(x) is significant only if (e,x) is in the relation target.
   */
  def targets: Relation[E, U]

  /**
   * The box to use when combining different layers.
   */
  def combine: Box[V]

  /**
   * Add boxes to the equation system in a localized way.
   * @param boxes new box to add.
   * @param ordering an order on unknown in order to decide for which layer we need to apply widening.
   * @param boxesAreIdempotent if true the boxes are assumed to be idempotent and some optimization is possible.
   */
  def withLocalizedBoxes(boxes: BoxAssignment[U, V], ordering: Ordering[U], boxesAreIdempotent: Boolean): LayeredEquationSystem[U, V, _]

  /**
   * Add an initial assignments to the equation system, which is combined with standard body.
   * @param rho the assignment to combine with the standard body.
   */
  def withInputAssignment(rho: PartialFunction[U, V]): FiniteEquationSystem[U, V]
}

object LayeredEquationSystem {
  import EquationSystem._

  /**
   * An alias for the type of the edgeBody method.
   */
  type EdgeBody[U, V, E] = E => (U => V) => (U => V)

  /**
   * Builds a layered equation system from a subset of its constituents.
   */
  def apply[U, V, E](unknowns: Iterable[U], inputUnknowns: Iterable[U], edgeBody: EdgeBody[U, V, E],
    sources: Relation[E, U], targets: Relation[E, U], initial: U => V, combine: Box[V]) =
    SimpleLayeredEquationSystem(unknowns, inputUnknowns, edgeBody, sources, targets, initial, combine)

  /**
   * An implementation of `LayeredEquationSystem` from a subset of its constituents.
   */
  case class SimpleLayeredEquationSystem[U, V, E](
      val unknowns: Iterable[U],
      val inputUnknowns: Iterable[U],
      val edgeBody: EdgeBody[U, V, E],
      val sources: Relation[E, U],
      val targets: Relation[E, U],
      val initial: U => V,
      val combine: Box[V]) extends LayeredEquationSystem[U, V, E] {

    private val ingoing = targets.inverse

    private val outgoing = sources.inverse

    def body = { (rho: U => V) =>
      (x: U) =>
        val contributions = for (e <- ingoing.image(x)) yield edgeBody(e)(rho)(x)
        // if contribution is empty the unknown x has no right hand side... it seems
        // reasonable to return the old value.
        if (contributions.isEmpty) rho(x) else contributions reduce { combine }
    }

    // TODO there is no way to force a particular ordering of the dependencies
    def bodyWithDependencies = { (rho: U => V) =>
      (x: U) =>
        val deps = for (e <- ingoing.image(x); y <- sources.image(e)) yield y
        val res = body(rho)(x)
        (res, deps)
    }

    val infl = {
      val graph = for (x <- unknowns; e <- ingoing.image(x); y <- sources.image(e)) yield (y, x)
      val unknownsSet = unknowns.toSet
      Relation(graph.toSet, unknownsSet, unknownsSet)
    }

    def withInputAssignment(init: PartialFunction[U, V]) = FiniteEquationSystem(
      body = addInputAssignmentToBody(body, init, combine),
      unknowns = unknowns,
      inputUnknowns = inputUnknowns,
      infl = infl,
      initial = initial)

    def withBoxes(boxes: BoxAssignment[U, V], boxesAreIdempotent: Boolean) = FiniteEquationSystem(
      body = addBoxesToBody(body, boxes),
      inputUnknowns = inputUnknowns,
      unknowns = unknowns,
      infl = if (boxesAreIdempotent) infl else infl union Relation(unknowns.toSet, { (u: U) => Set(u) }),
      initial = initial)

    def withLocalizedBoxes(boxes: BoxAssignment[U, V], ordering: Ordering[U], boxesAreIdempotent: Boolean) = {
      val newEdgeBody: EdgeBody[U, V, E] = { e: E =>
        (rho: U => V) => x: U =>
          if (boxes.isDefinedAt(x) && sources.image(e).exists { ordering.lteq(x, _) }) {
            boxes(x)(rho(x), edgeBody(e)(rho)(x))
          } else
            edgeBody(e)(rho)(x)
      }
      val newSources = if (boxesAreIdempotent)
        sources
      else {
        val newgraph: Set[(E, U)] = (for (x <- unknowns; e <- ingoing.image(x); if sources.image(e) exists { ordering.lteq(x, _) })
          yield (e, x))(collection.breakOut)
        sources union Relation(newgraph)
      }
      copy(edgeBody = newEdgeBody, sources = newSources)
    }
  }

  /**
   * This method takes a body, a partial assignment and a combine operation, and returns a new equation system
   * where the r.h.s. of each unknown is combined with init. In formulas, if `newbody` is the result of the method
   * and `init` is defined on 'x', then `newbody(rho)(x) = combine( init(x), body(rho)(x) )`.
   */
  def addInputAssignmentToBody[U, V](body: Body[U, V], init: PartialFunction[U, V], combine: Box[V]): Body[U, V] = {
    (rho: U => V) =>
      (x: U) => {
        if (init.isDefinedAt(x)) combine(init(x), body(rho)(x)) else body(rho)(x)
      }
  }
}
