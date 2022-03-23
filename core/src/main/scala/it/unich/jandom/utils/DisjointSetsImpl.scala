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

import scala.collection.mutable.{ Map => MMap }

/**
 * This class implements a disjoint-sets algorithm with
 * union-by-rank and path compression. The forest/sets/etc. are
 * internal data structures not exposed to the outside. Instead,
 * it is possible to add new elements (which end up in a newly
 * created set), join two elements, and hence, their sets, and
 * find the representative of a disjoint-set by a given element of
 * the set.
 *
 * The code has been take by the following StackExchange discussion:
 * http://codereview.stackexchange.com/questions/17621/scala-disjoint-sets
 */
class DisjointSetsImpl[T](initialElements: Seq[T]) extends DisjointSets[T] {

  /**
   * A map relating values in T to nodes
   */
  private val nodes: MMap[T, DisjointSetsImpl.Node[T]] = MMap.empty

  initialElements foreach (this += _)

  def clear() = nodes.clear()

  /**
   * Add a new single-node forest to the disjoint-set forests. It will
   * be placed into its own set.
   */
  def addOne(elem: T) = {
    nodes += (elem -> DisjointSetsImpl.Node(elem, 0, None))
    this
  }
  
  def size = nodes.size

  /**
   * Add a new single-node forest  to the disjoint-set forests. It will
   * be placed into its own set. It returns the new node.
   */
  private def add(elem: T) = {
    val node = DisjointSetsImpl.Node(elem, 0, None)
    nodes += elem -> node
    node
  }

  def union(elem1: T, elem2: T): T = {
    val n1 = nodes.get(elem1) match {
      case None => add(elem1)
      case Some(n) => n.getRepresentative
    }
    val n2 = nodes.get(elem2) match {
      case None => add(elem2)
      case Some(n) => n.getRepresentative
    }
    if (n1 == n2)
      n1.elem
    else if (n1.rank > n2.rank) {
      n2.parent = Some(n1)
      n1.elem
    } else if (n1.rank < n2.rank) {
      n1.parent = Some(n2)
      n2.elem
    } else {
      n1.rank += 1
      n2.parent = Some(n1)
      n1.elem
    }
  }

  def find(elem: T): Option[T] = nodes.get(elem) match {
    case Some(node) => Some(node.getRepresentative.elem)
    case None => None
  }

  def apply(elem: T) = find(elem).get

  def inSamePartition(elem1: T, elem2: T): Boolean = {
    val n1 = find(elem1)
    val n2 = find(elem2)
    if (n1.isDefined && n2.isDefined)
      n1.get == n2.get
    else
      false
  }
  
  /**
   * @inheritdoc
   * This is a non-vital/non-standard operation, so we do
   * not keep track of the number of sets, and instead this method recomputes
   * them each time.
   */
  def setCount: Int = nodes.values.count(_.parent.isEmpty)
  
  override def toString = nodes.values.mkString(", ") 
}

object DisjointSetsImpl {

  /**
   * Create an disjoint set with the specified initial elements
   */
  def apply[T](initialElements: T*) = new DisjointSetsImpl[T](initialElements)

  /**
   * The internal node structure
   */
  private case class Node[T](val elem: T, var rank: Int, var parent: Option[Node[T]]) {
    /**
     * Compute representative of this set, and also compresses paths
     * @return root element of the set
     */
    final def getRepresentative: Node[T] = parent match {
      case None => this
      case Some(p) =>
        val rep = p.getRepresentative
        parent = Some(rep)
        rep
    }
    
    override def toString = s"${elem} -${rank}-> ${if (parent.isDefined) parent.get.elem.toString else "X"}"
  }
}
 
