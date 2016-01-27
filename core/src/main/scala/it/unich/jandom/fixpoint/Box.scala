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

import it.unich.jandom.fixpoint.lattice.DirectedPartialOrdering
import it.unich.jandom.fixpoint.lattice.DirectedSet

/**
 * The `Box` object defines some factories for building boxes, i.e. functions of the
 * type (V,V)=>V for some V which are abstractions of widenings, narrowings, etc..
 */
object Box {
  /**
   * A box which always returns its right component (new contribution).
   */
  def right[V]: Box[V] = { (x: V, y: V) => y }

  /**
   * A box which always returns its left component (original value).
   */
  def left[V]: Box[V] = { (x: V, y: V) => x }

  /**
   * Returns the upper bound for a directed set as a Box.
   */
  def union[V <: DirectedSet[V]]: Box[V] = { _ union _ }

  /**
   * Warrowing, as defined in the upcoming paper of Amato, Scozzari, Seidl, Apinis, Vodjani,
   * "Efficiently intertwining widening and narrowing".
   * @tparam V the type of values, should be a partial ordered
   * @param widening is widening over V
   * @param narrowing is a narrowing over V
   */
  def warrowing[V <: PartiallyOrdered[V]](widening: Box[V], narrowing: Box[V]): Box[V] = {
    (x: V, y: V) => if (y <= x) narrowing(x, y) else widening(x, y)
  }

  /**
   * Warrowing, as defined in the upcoming paper of Amato, Scozzari, Seidl, Apinis, Vodjani,
   * "Efficiently intertwining widening and narrowing".
   * @tparam V the type of values, should be a partially ordered
   * @param widening is widening over V
   * @param narrowing is a narrowing over V
   */
  def warrowingFromOrdering[V: PartialOrdering](widening: Box[V], narrowing: Box[V]): Box[V] = {
    val order = implicitly[PartialOrdering[V]]
    (x: V, y: V) => if (order.lteq(y, x)) narrowing(x, y) else widening(x, y)
  }
}

