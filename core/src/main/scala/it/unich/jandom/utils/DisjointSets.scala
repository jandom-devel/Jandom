/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.utils

import scala.collection.generic.Growable

/**
 * This is a trait for disjoint sets. A disjoint set algorithm keeps a number
 * of disjoint sets and allow to create new sets, join two sets, and find if
 * two elements are in the same set.
 */
trait DisjointSets[T] extends Growable[T] {
  /**
   * Join the disjoint-sets of which <code>elem1</code>
   * and <code>elem2</code> are members of. If <code>elem1</code>
   * or <code>elem2</code> are not part of the disjoint sets,
   * they are added before performing the union.
   *
   * We also require that, if union(elem1, elem2) is always called in such a way that
   * elem1 enjoys property P, then the representative of all those elements which have been joined
   * at least once has the property P.
   * @return the representative of the joined sets
   */
  def union(elem1: T, elem2: T): T

  /**
   * Finds the representative for a disjoint-set, of which
   * <code>elem</code> is a member of.
   */
  def find(elem: T): Option[T]

  /**
   * Find the representative for a disjoint-set, of which
   * <code>elem</code> is a member of.
   * @throws NoSuchElementException if the element is not in any partition
   */
  def apply(elem: T): T

  /**
   * Returns the number of disjoint-sets managed in this data structure.
   */
  def setCount: Int

  /**
   * Returns the number of elements in the data-structure
   */
  def size: Int

  /**
   * Returns whether two elements are in the same partition
   */
  def inSamePartition(elem1: T, elem2: T): Boolean
}

/**
 * This is the compaion object for the trait DisjointSets
 */
object DisjointSets {
  def apply[T](initialElements: T*) = new DisjointSetsImpl[T](initialElements)
  def empty[T] = new DisjointSetsImpl[T](Seq.empty)
}
