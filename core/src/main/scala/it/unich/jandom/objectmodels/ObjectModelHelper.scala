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

package it.unich.jandom.objectmodels

import scala.annotation.tailrec

/**
 * This trait defines concrete methods which may be used to implement an object model. It is not
 * particularly fast at the moment, since correctness and readability has been favored rather than
 * performance. However, memoization is used to improve performance in some particularly lengthy
 * computations.
 * @todo make the methods faster
 */
trait ObjectModelHelper {
  this: ObjectModel =>

  /**
   * A mutable HashMap used for memoizing sharing information.
   */
  private val sharing = collection.mutable.HashMap[(Type, Type), Boolean]()

  /**
   * A mutable HashMap used for memoizing reachability information.
   */
  private val reachable = collection.mutable.HashMap[Type, Set[Type]]()

  /**
   * A mutable HashMap used for memoizing concreteApproximations
   */
  private val glb = collection.mutable.HashMap[(Type, Type), Option[Type]]()

  def pathExists(t: Type, fs: Field*): Boolean = {
    if (fs.isEmpty)
      true
    else {
      val f = fs.head
      (fields(t) contains f) && pathExists(t, fs.tail: _*)
    }
  }

  /**
   * This is an helper methods which visit an implicit graph, collecting values associated to nodes.
   * @tparam Node the type for nodes of the graph
   * @tparam Value the values associated to nodes
   * @param start starting node
   * @param children maps each node to its children
   * @param values maps each node to a set of values
   */
  protected def visitGraph[Node, Value](start: Node, children: Node => Set[Node], values: Node => Set[Value]): Set[Value] = {
    val worklist = collection.mutable.Queue[Node](start)
    val result = collection.mutable.Set[Value]()
    while (!worklist.isEmpty) {
      val current = worklist.dequeue()
      result ++= values(current)
      worklist ++= children(current)
    }
    result.toSet
  }

  def ancestors(t: Type): Set[Type] = visitGraph(t, parents, { (t: Type) => Set(t) })

  def descendants(t: Type): Set[Type] = visitGraph(t, children, { (t: Type) => Set(t) })

  def fields(t: Type): Set[Field] = visitGraph(t, parents, declaredFields)

  def possibleFields(t: Type): Set[Field] = {
    for ( k <- descendants(t); if isConcrete(k); f <- fields(k) )
      yield f
  }

  def isConcretizable(t: Type): Boolean = {
    descendants(t) exists isConcrete
  }

  def upperCrown(ts: Iterable[Type]): Set[Type] = {

    @tailrec
    def upperCrownHelper(ts: Iterable[Type], acc: Set[Type]): Set[Type] = {
      if (ts.isEmpty)
        acc
      else {
        val newt = ts.head
        val newacc = collection.mutable.Set[Type]()
        var toAdd = true
        for (t <- acc) {
          if (lteq(newt, t)) toAdd = false
          if ((!lteq(t, newt)) || t == newt) newacc += t
        }
        if (toAdd) newacc += newt
        upperCrownHelper(ts.tail, newacc.toSet)
      }
    }
    upperCrownHelper(ts, Set())
  }

  def concreteApprox(t1: Type, t2: Type): Option[Type] = glb.getOrElseUpdate((t1, t2), {
    if (lteq(t1, t2) && isConcrete(t1))
      Option(t1)
    else {
      val glbs = upperCrown(children(t1) map { concreteApprox(_, t2) } filter { _.isDefined } map { _.get })
      // Actually, we would need to remove elements from glbs which are subsumed by other elements.
      if (glbs.isEmpty)
        None
      else if (glbs.forall(_ == glbs.head))
        Option(glbs.head)
      else
        Option(t1)
    }
  })

  /**
   * @inheritdoc
   * It is  computed by iterating the binary glbApprox, but may be overriden for performance reasons.
   */
  def concreteApprox(ts: Iterable[Type]): Option[Type] = {

    @tailrec
    def glbhelper(ts: Iterable[Type], current: Option[Type]): Option[Type] = {
      if (ts.isEmpty || current.isEmpty)
        current
      else
        glbhelper(ts.tail, concreteApprox(current.get, ts.head))
    }
    if (ts.isEmpty)
      None
    else
      glbhelper(ts, Option(ts.head))
  }

  def concreteApprox(t: Type): Option[Type] = {
    var subs = Set(t)
    var current = t
    do {
      current = subs.head
      subs = upperCrown((children(current) filter isConcretizable) map { concreteApprox(_).get })
    } while (subs.size == 1 && (!isConcrete(current)))
    if (isConcretizable(current))
      Option(current)
    else
      None
  }

  def neededFields(t: Type): Set[Field] = {
    val glb = concreteApprox(t, t)
    if (glb.isEmpty) Set() else fields(glb.get)
  }

  def reachablesFrom(t: Type): Set[Type] = reachable.get(t) match {
    case Some(types) =>
      types
    case None =>
      reachable(t) = Set()
      val set = collection.mutable.Set[Type]()
      val queue = collection.mutable.Queue[Type](t)
      while (queue.nonEmpty) {
        val t1 = queue.dequeue
        if (isConcretizable(t1) && !isPrimitive(t1)) set += t1
        for { f <- possibleFields(t1); t2 = typeOf(f); if !set.contains(t2) } queue.enqueue(t2)
        for { elt <- elementType(t1) } queue.enqueue(elt)
      }
      val result = set.toSet
      reachable(t) = result
      result
  }

  def isReachable(src: Type, tgt: Type) = reachablesFrom(src) exists { lteq(tgt, _) }

  def mayBeAliases(t1: Type, t2: Type): Boolean =
    !isPrimitive(t1) && !isPrimitive(t2) && concreteApprox(t1, t2).isDefined

  def mayShare(t1: Type, t2: Type): Boolean = {
    val doShare = sharing.get((t1, t2)) orElse sharing.get((t2, t1))
    if (doShare.isDefined)
      doShare.get
    else {
      val reach1 = reachablesFrom(t1)
      val reach2 = reachablesFrom(t2)
      val sharable = reach1 exists { t1 => reach2 exists { t2 => mayBeAliases(t1, t2) } }
      sharing((t1, t2)) = sharable
      sharing((t2, t1)) = sharable
      sharable
    }
  }

}
