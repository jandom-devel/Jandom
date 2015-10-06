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

import scala.language.implicitConversions
import it.unich.jandom.fixpoint.lattice.DirectedSet
import it.unich.jandom.fixpoint.lattice.DirectedPartialOrdering

/**
 * A BoxAssignment maps a subset of the program's points to a Box. When isDefinedAtu(u)
 * is false for a box assignment, the corresponding apply(u) should be equal to the
 * right box.
 */
abstract class BoxAssignment[-U,V] extends PartialFunction[U, (V,V)=>V] {
  outer =>

  /**
   * Restrict the domain of this box assignment. The new domain is the intersection of 
   * the old domain and the set whose characteristic function is `domain`
   */
  def restrict[U1 <: U](domain: U1 => Boolean) = new BoxAssignment[U1, V] {
    def apply(u: U1) = outer(u)
    def isDefinedAt(u: U1) = domain(u) && outer.isDefinedAt(u)
  }
}

/**
 * The `BoxAssignment` object defines some factories for building box assignments.
 */

object BoxAssignment {
  /**
   * Given a box return an assignment which assign each program point to that box.
   */
  implicit def constant[V](box: Box[V]) = new BoxAssignment[Any, V] {
    def apply(u: Any) = box    
    def isDefinedAt(u: Any) = true
  }
  
  def right[V] = new BoxAssignment[Any,V] {
    def apply(u: Any) = { (x:V, y:V) => y }
    def isDefinedAt(u: Any) = false
  }
  
  def left[V] = new BoxAssignment[Any,V] {
    def apply(u: Any) = { (x:V, y:V) => x }
    def isDefinedAt(u: Any) = true
  }
  
  /**
   * Returns the upper bound for a directed set.
   */
  def upperbound[V <: DirectedSet[V]]: BoxAssignment[Any,V] = { (x:V, y:V) => x upperbound y }

  /**
   * A box which coincides with the `first` box for `delay` calls, and then coincides with the `second` box.
   * This may be used to implement delayed widening and narrowing. The definition domain is the intersection
   * of the definition domain of both box assignments.
   */
  def cascade[U, V](first: BoxAssignment[U, V], delay: Int, second: BoxAssignment[U, V]): BoxAssignment[U, V] = {
    require(delay >= 0)
    new BoxAssignment[U, V] {
      private val delays = collection.mutable.Map.empty[U, Int].withDefaultValue(0)
      def isDefinedAt(u: U) = first.isDefinedAt(u) || second.isDefinedAt(u)
      def apply(u: U) = {
        val steps = delays(u)
        if (steps < delay) {
          delays(u) += 1
          first(u)
        } else
          second(u)
      }
    }
  }

  /**
   * Returns a delayed widening obtained by returning the right argument for a given number of steps
   * before using the provided box. Note that a delayedBox with a widening is not a widening, since it
   * is not always an upper bound of the two arguments. However, a delayedBox with a narrowing is
   * a narrowing.
   */
  def delayedBox[U, V](box: BoxAssignment[U, V], delay: Int): BoxAssignment[U, V] =
    cascade(right[V], delay, box)

  /**
   * Returns a delayed widening obtained by returning the right argument for a given number of steps
   * before using the provided narrowing. It is the same of delayedBox.
   */
  def delayedNarrowing[U, V](narrowing: BoxAssignment[U, V], delay: Int): BoxAssignment[U, V] =
    cascade(right[V], delay, narrowing)

  /**
   * Returns a delayed widening obtained by applying upperbound for a given number of steps
   * before using the provided widening.
   */
  def delayedWidening[U, V <: DirectedSet[V]](widening: BoxAssignment[U, V], delay: Int): BoxAssignment[U, V] =
    cascade({ (x:V, y:V) => x upperbound y }, delay, widening)

  /**
   * Returns a delayed widening obtained by applying upperbound for a given number of steps
   * before using the provided widening.
   */
  def delayedWidening[U, V: DirectedPartialOrdering](widening: BoxAssignment[U, V], delay: Int): BoxAssignment[U, V] =
    cascade(implicitly[DirectedPartialOrdering[V]].upperbound _, delay, widening)

  /**
   * Warrowing, as defined in the upcoming paper of Amato, Scozzari, Seidl, Apinis, Vodjani,
   * "Efficiently intertwining widening and narrowing". The definition domain is the intersection
   * of the definition domain of both box assignments.
   * @tparam V the type of values, should be a partial ordered
   * @param widening is widening over V
   * @param narrowing is a narrowing over V
   */
  def warrowing[U, V <: PartiallyOrdered[V]](widening: BoxAssignment[U, V], narrowing: BoxAssignment[U, V]) = new BoxAssignment[U, V] {
    def apply(u: U) = { (x: V, y: V) => if (y <= x) narrowing(u)(x, y) else widening(u)(x, y) }
    def isDefinedAt(u: U) = widening.isDefinedAt(u) || narrowing.isDefinedAt(u)
  }

  /**
   * Warrowing, as defined in the upcoming paper of Amato, Scozzari, Seidl, Apinis, Vodjani,
   * "Efficiently intertwining widening and narrowing". The definition domain is the intersection
   * of the definition domain of both box assignments.
   * @tparam V the type of values, should be a partially ordered
   * @param widening is widening over V
   * @param narrowing is a narrowing over V
   */
  def warrowingFromOrdering[U, V: PartialOrdering](widening: BoxAssignment[U, V], narrowing: BoxAssignment[U, V]) = new BoxAssignment[U, V] {
    def apply(u: U) = { (x: V, y: V) => if (implicitly[PartialOrdering[V]].lteq(x,y)) narrowing(u)(x, y) else widening(u)(x, y) }
    def isDefinedAt(u: U) = widening.isDefinedAt(u) || narrowing.isDefinedAt(u)
  }
}
