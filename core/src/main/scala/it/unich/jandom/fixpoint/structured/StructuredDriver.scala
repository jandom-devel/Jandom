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
import it.unich.jandom.utils.Relation

/**
 * This driver is an interface for solvers of layered equation systems.
 */
object StructuredDriver extends Driver {

  /**
   * Returns the standard widening for the type V.
   */
  private def defaultWidening[V <: AbstractProperty[V]] = { (x:V, y:V) => x widening y }

  /**
   * Returns the standard union for the type V.
   */
  private def unionWidening[V <: AbstractProperty[V]] = { (x:V, y:V) => x union y }

  /**
   * Returns the standard narrowing for the type V.
   */
  private def defaultNarrowing[V <: AbstractProperty[V]] = { (x:V, y:V) => x narrowing y }

  /**
   * Returns the standard intersection for the type V.
   */
  private def intersectionNarrowing[V <: AbstractProperty[V]] = { (x:V, y:V) => x intersection y }

  /**
   * Returns an assignment of a widening for each unknown.
   * @param w input parameter which drives the generation of the widening assignment.
   */
  private def wideningDefine[V <: AbstractProperty[V]](w: Widenings.Widening): BoxAssignment[Any, V] = {
    w match {
      case Widenings.Default => defaultWidening[V]
      case Widenings.Union => unionWidening[V]
      case Widenings.None => BoxAssignment.right[V]
      case Widenings.Delayed(first, delay, next) => BoxAssignment.cascade(wideningDefine[V](first), delay, wideningDefine[V](next))
    }
  }

  /**
   * Returns an assignment of a narrowing for each unknown.
   * @param b input parameter which drives the generation of the narrowing assignment.
   */
  private def narrowingDefine[V <: AbstractProperty[V]](n: Narrowings.Narrowing): BoxAssignment[Any, V] = {
    n match {
      case Narrowings.Default => defaultNarrowing[V]
      case Narrowings.Intersection => intersectionNarrowing[V]
      case Narrowings.Stop => BoxAssignment.left[V]
      case Narrowings.None => BoxAssignment.right[V]
      case Narrowings.Delayed(first, delay, next) => BoxAssignment.cascade(narrowingDefine[V](first), delay, narrowingDefine[V](next))
    }
  }

  /**
   * Returns an assignment of a mixed box for each unknown.
   * @param u input parameter which drives the generation of the mixed assignment.
   */
  private def warrowingDefine[V <: AbstractProperty[V]](u: Updates.Update): BoxAssignment[Any, V] = {
    u match {
      case Updates.DefaultUpdate => BoxAssignment.warrowing(defaultWidening[V], defaultNarrowing[V])
      case Updates.Combine(widening, narrowing) => BoxAssignment.warrowing(wideningDefine[V](widening), narrowingDefine[V](narrowing))
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
  private def boxFilter[U, V <: AbstractProperty[V]](eqs: FiniteEquationSystem[U, V], boxes: BoxAssignment[U, V], location: BoxLocation.Value, ordering: Option[GraphOrdering[U]]): Option[BoxAssignment[U, V]] =
    location match {
      case BoxLocation.None => None
      case BoxLocation.All => Some(boxes)
      case BoxLocation.Loop => Some(boxes.restrict(ordering.get.isHead))
    }

  /**
   * Apply a given box assignment to an equation system, generatig a new equation system.
   * @param eqs the equation system
   * @param boxes an (iterable) assignment of boxes to unknowns
   * @param scope an input parameters which determines how we want to apply boxes (such as localized or standard)
   * @param ordering an optional ordering on unknowns to be used for localized boxes.
   */
  private def boxApply[U, V <: AbstractProperty[V], E](eqs: LayeredEquationSystem[U, V, E], optBoxes: Option[BoxAssignment[U, V]], scope: BoxScope.Value, ordering: Option[Ordering[U]], idempotent: Boolean): FiniteEquationSystem[U, V] = {
    if (optBoxes.isEmpty)
      eqs
    else scope match {
      case BoxScope.Standard => eqs.withBoxes(optBoxes.get, idempotent)
      case BoxScope.Localized => eqs.withLocalizedBoxes(optBoxes.get, ordering.get, idempotent)
    }
  }

  def addLocalizedBoxes[U, V <: AbstractProperty[V], E](eqs: LayeredEquationSystem[U, V, E], widening: BoxAssignment[U, V], narrowing: BoxAssignment[U, V], ordering: Ordering[U], boxesAreIdempotent: Boolean): FiniteEquationSystem[U, V] = {
    val ingoing = eqs.targets.inverse
    val newbody = { (rho: U => V) =>
      (x: U) =>
        val contributions = for (e <- ingoing.image(x)) yield {
          val contrib = eqs.edgeBody(e)(rho)(x)
          val boxapply = eqs.sources.image(e).exists { ordering.lteq(x, _) } && !(contrib <= rho(x))
          (contrib, boxapply)
        }
        // if contribution is empty the unknown x has no right hand side... it seems
        // reasonable to return the old value.
        if (contributions.isEmpty)
          rho(x)
        else {
          val result = contributions reduce { (x: (V, Boolean), y: (V, Boolean)) => (eqs.combine(x._1, y._1), x._2 || y._2) }
          //println((x, rho(x), contributions))
          if (result._2) {
            widening(x)(rho(x), result._1)
          } else if (result._1 < rho(x)) narrowing(x)(rho(x), result._1) else result._1
        }
    }
    FiniteEquationSystem(
      body = newbody,
      inputUnknowns = eqs.inputUnknowns,
      unknowns = eqs.unknowns,
      infl = if (boxesAreIdempotent) eqs.infl else eqs.infl union Relation(eqs.unknowns.toSet, { (u: U) => Set(u) }),
      initial = eqs.initial)
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
        val widening = boxFilter[U, V](eqs, wideningDefine[V](p(Driver.widening)), boxlocation, ordering)
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
        if (boxscope == BoxScope.Localized) {
          val widening = boxFilter[U, V](eqs, wideningDefine(p(Driver.widening)), boxlocation, ordering) 
          val narrowing = boxFilter[U, V](eqs, narrowingDefine(p(Driver.narrowing)), boxlocation, ordering)
          val withUpdate = if (widening.isEmpty && narrowing.isEmpty) 
            eqs
          else 
            addLocalizedBoxes(eqs, widening getOrElse BoxAssignment.right[V], narrowing getOrElse BoxAssignment.right[V], ordering.get, false)
          FiniteDriver(withUpdate, eqs.initial, ordering, restart, p)
        } else {
          val warrowingAssignment = boxFilter[U, V](eqs, warrowingDefine(p(Driver.update)), boxlocation, ordering)
          // the mixed operator has no sense at the localized level
          val eqsWithWarrowing = boxApply(eqs, warrowingAssignment, boxscope, ordering, false)
          FiniteDriver(eqsWithWarrowing, eqs.initial, ordering, restart, p)
        }
    }
  }
}
