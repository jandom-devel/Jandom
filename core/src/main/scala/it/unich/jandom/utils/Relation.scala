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

package it.unich.jandom.utils

import scala.collection.mutable.MultiMap

/**
 * A trait representing a mathematical relation.
 * @tparam U the domain of the relation.
 * @tparam V the codomain of the relation.
 */
trait Relation[U, V] extends PartiallyOrdered[Relation[U, V]] {
  /**
   * Returns whether `u` is in relation to `v`.
   */
  def apply(u: U, v: V): Boolean

  /**
   * Returns whether the relation is empty.
   */
  def isEmpty: Boolean

  /**
   * Returns the graph of the relation.
   */
  def graph: Set[(U, V)]

  /**
   * Returns the image of `u` through the relation.
   */
  def image(u: U): Set[V]

  /**
   * Returns the elements in `U` which are part of the relation. Formally,
   * the term is not completely correct.
   */
  def domain: Set[U]

  /**
   * Returns the elements in `V` which are part of the relation. Formally,
   * the term is not completely correct.
   */
  def codomain: Set[V]

  /**
   * Returns the inverse relation.
   */
  def inverse: Relation[V, U]

  /**
   * The union of two relations.
   */
  def union(r: Relation[U, V]) = Relation.union(this, r)
}

/**
 * The companion object of FiniteRelation, with methods to create relations.
 */
object Relation {
  /**
   * This is a trait which may be mixed in with a FiniteRelation in order to automatically
   * derive the inverse relation.
   */
  trait AutomaticInverse[U, V] {
    original: Relation[U, V] =>

    lazy val inverse = {
      val memo = new collection.mutable.HashMap[V, collection.mutable.Set[U]] with MultiMap[V, U]
      for (u <- original.domain; v <- original.image(u)) memo.addBinding(v, u)
      val map = memo.toMap

      new Relation[V, U] with AutomaticPartialOrdering[V, U] {
        def isEmpty = original.isEmpty
        def apply(v: V, u: U) = map.getOrElse(v, Set.empty[U]) contains u
        def image(v: V) = map.getOrElse(v, Set.empty[U]).toSet
        lazy val graph = for { v <- map.keySet; u <- map(v) } yield (v, u)
        lazy val domain = map.keySet
        lazy val codomain = original.domain
        def inverse = original
      }
    }
  }

  /**
   * This is a trait which may be mixed in with a Relation to automatically derive
   * compare and equality functions.
   */
  trait AutomaticPartialOrdering[U, V] extends PartiallyOrdered[Relation[U,V]] {
    original: Relation[U, V] =>

    def tryCompareTo[B >: Relation[U, V]](that: B)(implicit arg0: (B) ⇒ PartiallyOrdered[B]): Option[Int] = {
      that match {
        case that: Relation[U, V] =>
          val thisgraph = graph
          val thatgraph = that.graph
          if (thisgraph == thatgraph)
            Some(0)
          else if (thisgraph subsetOf thatgraph)
            Some(-1)
          else if (thatgraph subsetOf thisgraph)
            Some(1)
          else None
        case _ => None
      }
    }

    override def equals(that: Any): Boolean = {
      that match {
        case that: Relation[U, V] => graph == that.graph
        case _ => false
      }
    }
  }

  /**
   * Builds a relation from U to V given a map from U to Set[V].
   */
  def apply[U, V](map: Map[U, Set[V]]): Relation[U, V] = new Relation[U, V] with AutomaticInverse[U, V] with AutomaticPartialOrdering[U, V] {
    def isEmpty = map.forall { _._2.isEmpty }
    def apply(u: U, v: V) = map.getOrElse(u, Set.empty[V]) contains v
    def image(u: U) = map.getOrElse(u, Set.empty[V])
    lazy val graph = for { u <- map.keySet; v <- map(u) } yield (u, v)
    def domain = map.keySet
    def codomain = inverse.domain
  }

  /**
   * Builds a relation from U to V given a set of elements in U (the domain of the relation) and a function
   * from U to Set[V].
   * @param dom the domain of the relation
   * @param f the image function
   */
  def apply[U, V](dom: Set[U], f: U => Set[V]): Relation[U, V] = new Relation[U, V] with AutomaticInverse[U, V] with AutomaticPartialOrdering[U, V] {
    def isEmpty = dom.forall { f(_).isEmpty }
    def apply(u: U, v: V) = f(u) contains v
    def image(u: U) = f(u)
    lazy val graph = for { u <- dom; v <- f(u) } yield (u, v)
    def domain = dom
    def codomain = inverse.domain
  }

  /**
   * Builds a relation from its graph. Domain and codomain are computed from the graph.
   */
  def apply[U, V](aGraph: Set[(U, V)]): Relation[U, V] = apply(aGraph, aGraph.map(_._1), aGraph.map(_._2))

  /**
   * Builds a relation from its graph. Domains and codomain are provided explicitly.
   */
  def apply[U, V](aGraph: Set[(U, V)], aDomain: Set[U], aCodomain: Set[V]): Relation[U, V] = new Relation[U, V] with AutomaticInverse[U, V] with AutomaticPartialOrdering[U, V] {
    def isEmpty = aGraph.isEmpty
    def graph = aGraph
    def image(u: U) = aGraph.filter(_._1 == u).map(_._2)
    def apply(u: U, v: V) = aGraph contains ((u, v))
    def domain = aDomain
    def codomain = aCodomain
  }

  /**
   * Returns a new relation which is the union of two different relations.
   */
  def union[U, V](r1: Relation[U, V], r2: Relation[U, V]): Relation[U, V] = new Relation[U, V] with AutomaticInverse[U, V] with AutomaticPartialOrdering[U, V] {
    def isEmpty = r1.isEmpty && r2.isEmpty
    def graph = r1.graph union r2.graph
    def image(u: U) = r1.image(u) ++ r2.image(u)
    def apply(u: U, v: V) = r1(u, v) || r2(u, v)
    def domain = r1.domain ++ r2.domain
    def codomain = r1.codomain ++ r2.codomain
  }

  /**
   * Helper methods to builds a relation from its graph expressed as list of elements.
   */
  def apply[U, V](graph: (U, V)*): Relation[U, V] = apply(Set(graph: _*))

  /**
   * Returns an empty relation.
   */
  def empty[U, V] = apply(Set.empty[(U, V)])
}
