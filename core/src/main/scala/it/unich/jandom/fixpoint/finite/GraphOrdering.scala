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

package it.unich.jandom.fixpoint.finite

/**
 * A GraphOrdering is an ordering on objects of the type `T` (which should be thought of
 * as nodes of a graph), where for each object we mark whether it is an head element or not.
 * Note that, a graph ordering generally considers only a subset of the elements of type `T`,
 * those returned by the `toSeq` method. The result of applying any method of this trait
 * on any element which is not part of the domain is not specified.
 * @param T the type of the ordered element.
 */
trait GraphOrdering[T] extends Ordering[T] {
  /**
   * Returns the elements in the correct order.
   */
  def toSeq: Seq[T]

  /**
   * It returns whether `u` is an head element.
   */
  def isHead(x: T): Boolean

  /**
   * Returns the set of head nodes.
   */
  def heads: Set[T]

  /**
   * Converts a GraphOrdering into a string composed by the sequence of its elements in the correct order. Head
   * elements are marked with parenthesis.
   */
  override def toString = (toSeq map { x => if (isHead(x)) x.toString else "(" + x.toString + ")" }).mkString("[ ", " ", " ]")
}

object GraphOrdering {
  /**
   * A graph ordering where each element is an head, and the order is given by the sequence `elements`.
   */
  case class TrivialGraphOrdering[T](val elements: Seq[T]) extends GraphOrdering[T] {
    def toSeq = elements
    def isHead(x: T) = true
    def heads = toSeq.toSet
    def compare(x: T, y: T) = elements.indexOf(x) - elements.indexOf(y)
  }

  /**
   * Build a graph ordering where each element is an head, and the order is given by the sequence of `elements`.
   */
  def apply[T](elements: T*) = TrivialGraphOrdering(elements)
}