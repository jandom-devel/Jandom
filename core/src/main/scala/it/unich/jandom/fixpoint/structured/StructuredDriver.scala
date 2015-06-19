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

package it.unich.jandom.fixpoint.structured

import it.unich.jandom._
import it.unich.jandom.domains._
import it.unich.jandom.fixpoint._
import it.unich.jandom.fixpoint.Driver._
import it.unich.jandom.fixpoint.FixpointSolverListener.EmptyListener
import it.unich.jandom.fixpoint.finite._
import it.unich.jandom.utils.IterableFunction
import it.unich.jandom.utils.PMaps._

/**
 * This driver is an interface for solvers of layered equation systems.
 */
object StructuredDriver extends it.unich.jandom.fixpoint.Driver {

  /**
   * Returns the standard widening for the type V.
   */
  private def defaultWidening[V <: AbstractProperty[V]]: Box[V] = { _ widening _ }

  /**
   * Returns the standard union for the type V.
   */
  private def unionWidening[V <: AbstractProperty[V]]: Box[V] = { _ union _ }

  /**
   * Returns the standard narrowing for the type V.
   */
  private def defaultNarrowing[V <: AbstractProperty[V]]: Box[V] = { _ narrowing _ }

  /**
   * Returns the standard intersection for the type V.
   */
  private def intersectionNarrowing[V <: AbstractProperty[V]]: Box[V] = { _ intersection _ }

  /**
   * Returns a mixed box from a given widening and narrowing.
   */
  private def createUpdate[V <: AbstractProperty[V]](widening: Box[V], narrowing: Box[V]): Box[V] = { (x: V, y: V) => if (y <= x) narrowing(x, y) else widening(x, y) }

  /**
   * Returns an assignment of a widening for each unknown.
   * @param w input parameter which drives the generation of the widening assignment.
   */
  private def wideningDefine[V <: AbstractProperty[V]](w: Widenings.Widening): Any => Box[V] = {
    w match {
      case Widenings.Default => { _ => defaultWidening }
      case Widenings.Union => { _ => unionWidening }
      case Widenings.None => { _ => Box.right[V] }
      case Widenings.Delayed(first, delay, next) =>
        val hash = collection.mutable.Map.empty[Any, Box[V]]
        val widening1 = wideningDefine[V](first)
        val widening2 = wideningDefine[V](next)
        val widening = { (x: Any) => hash.getOrElseUpdate(x, Box.cascade(widening1(x), delay, widening2(x))) }
        widening
    }
  }

  /**
   * Returns an assignment of a narrowing for each unknown.
   * @param b input parameter which drives the generation of the narrowing assignment.
   */
  private def narrowingDefine[V <: AbstractProperty[V]](n: Narrowings.Narrowing): Any => Box[V] = {
    n match {
      case Narrowings.Default => { _ => defaultNarrowing }
      case Narrowings.Intersection => { _ => intersectionNarrowing }
      case Narrowings.Stop => { _ => Box.left[V] }
      case Narrowings.None => { _ => Box.right[V] }
      case Narrowings.Delayed(first, delay, next) =>
        val hash = collection.mutable.Map.empty[Any, Box[V]]
        val narrowing1 = narrowingDefine[V](first)
        val narrowing2 = narrowingDefine[V](next)
        val narrowing = { (x: Any) => hash.getOrElseUpdate(x, Box.cascade(narrowing1(x), delay, narrowing2(x))) }
        narrowing
    }
  }

  /**
   * Returns an assignment of a mixed box for each unknown.
   * @param u input parameter which drives the generation of the mixed assignment.
   */
  private def updateDefine[V <: AbstractProperty[V]](u: Updates.Update): Any => Box[V] = {
    u match {
      case Updates.DefaultUpdate => {
        _ => createUpdate(defaultWidening, defaultNarrowing)
      }
      case Updates.Combine(widening, narrowing) => {
        val w = wideningDefine[V](widening)
        val n = narrowingDefine[V](narrowing)
        val update = { (x: Any) => createUpdate(w(x), n(x)) }
        update
      }
    }
  }

  /**
   * Given an equation system and a box assignment, filter the assignment according to what specified in the input parameter location and
   * the graph ordering.
   * @param eqs an equation system
   * @param boxes a box assignment
   * @param location input parameter which drives the filtering by specifying where to put boxes.
   * @param ordering a GraphOrdering used when we need to detect heads.
   */
  private def boxFilter[U, V <: AbstractProperty[V]](eqs: FiniteEquationSystem[U, V], boxes: U => Box[V], location: BoxLocation.Value, ordering: Option[GraphOrdering[U]]): IterableFunction[U, Box[V]] = {
    location match {
      case BoxLocation.None =>
        IterableFunction.empty
      case BoxLocation.All =>
        new IterableFunction[U, Box[V]] {
          def apply(x: U) = boxes(x)
          def isDefinedAt(x: U) = true
          def iterator = eqs.unknowns.iterator map { u => (u, boxes(u)) }
          def keys = eqs.unknowns
          def keySet = eqs.unknowns.toSet
        }
      case BoxLocation.Loop =>
        val realOrdering = ordering.get
        new IterableFunction[U, Box[V]] {
          def apply(x: U) = boxes(x)
          def isDefinedAt(x: U) = realOrdering.isHead(x)
          def iterator = eqs.unknowns.iterator filter realOrdering.isHead map { u => (u, apply(u)) }
          def keys = eqs.unknowns filter realOrdering.isHead
          def keySet = keys.toSet
        }
    }
  }

  /**
   * Apply a given box assignment to an equation system, generatig a new equation system.
   * @param eqs the equation system
   * @param boxes an (iterable) assignment of boxes to unknowns
   * @param scope an input parameters which determines how we want to apply boxes (such as localized or standard)
   * @param ordering an optional ordering on unknowns to be used for localized boxes.
   */
  private def boxApply[U, V, E](eqs: LayeredEquationSystem[U, V, E], boxes: IterableFunction[U, Box[V]], scope: BoxScope.Value, ordering: Option[Ordering[U]], idempotent: Boolean): FiniteEquationSystem[U, V] = {
    if (boxes.isEmpty)
      eqs
    else scope match {
      case BoxScope.Standard => eqs.withBoxes(boxes, idempotent)
      case BoxScope.Localized => eqs.withLocalizedBoxes(boxes, ordering.get, idempotent)
    }
  }

  /**
   * Solves the equation system using the parameters specified in p.
   * @param eqs the equation system to solve
   * @param p parameters passed through a PMap
   */
  def apply[U, E](dom: AbstractDomain)(eqs: LayeredEquationSystem[U, dom.Property, E], p: PNil): U => dom.Property = {
    type V = dom.Property

    val solver = p(Driver.solver)
    val boxlocation = p(Driver.boxlocation)
    val boxstrategy = p(Driver.boxstrategy)
    val boxscope = p(Driver.boxscope)
    val restartstrategy = p(Driver.restartstrategy)

    val ordering1: Option[GraphOrdering[U]] = (solver, boxscope) match {
      case (Solver.HierarchicalOrderingSolver, _) =>
        Some(HierarchicalOrdering(DFOrdering(eqs.infl, eqs.inputUnknowns)))
      case (Solver.PriorityWorkListSolver, _) | (_, BoxScope.Localized) =>
        Some(DFOrdering(eqs.infl, eqs.inputUnknowns))
      case _ =>
        None
    }

    val ordering: Option[GraphOrdering[U]] = boxlocation match {
      case BoxLocation.None | BoxLocation.All =>
        None
      case BoxLocation.Loop =>
        ordering1 orElse Some(DFOrdering(eqs.infl, eqs.inputUnknowns))
    }

    val restart: (dom.Property, dom.Property) => Boolean =
      if (restartstrategy) { (x, y) => x < y }
      else { (x, y) => false }

    boxstrategy match {
      case BoxStrategy.OnlyWidening =>
        val widening = boxFilter[U, V](eqs, wideningDefine(p(Driver.widening)), boxlocation, ordering)
        val withWidening = boxApply(eqs, widening, boxscope, ordering, true)
        FiniteDriver(withWidening, eqs.initial, ordering, restart, p)
      case BoxStrategy.TwoPhases =>
        val widening = boxFilter[U, V](eqs, wideningDefine(p(Driver.widening)), boxlocation, ordering)
        val withWidening = boxApply(eqs, widening, boxscope, ordering, true)
        val ascendingAssignment = FiniteDriver(withWidening, eqs.initial, ordering, restart, p)
        val narrowing = boxFilter[U, V](eqs, narrowingDefine(p(Driver.narrowing)), boxlocation, ordering)
        // localizing narrowing does not seem useful
        val withNarrowing = boxApply(eqs, narrowing, BoxScope.Standard, ordering, true)
        FiniteDriver(withNarrowing, ascendingAssignment, ordering, restart, p)
      case BoxStrategy.Mixed =>
        val update = boxFilter[U, V](eqs, updateDefine(p(Driver.update)), boxlocation, ordering)
        val withUpdate = boxApply(eqs, update, boxscope, ordering, false)
        FiniteDriver(withUpdate, eqs.initial, ordering, restart, p)
    }
  }
}
