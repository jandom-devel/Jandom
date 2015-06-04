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
 * Hierarchical ordering as defined in Bourdoncle's paper "Efficient chaotic iteration strategies with widenings", FMPA'93.
 * Note that, a hierarchical ordering generally considers only a subset of the elements of type T, those
 * returned by the `domain` method. The result of applying any method of this trait on any element which is not part of
 * the domain is not specified.
 */
trait HierarchicalOrdering[T] extends Ordering[T] {
  /**
   * Returns the elements in the correct ordering.
   */
  def toSeq: Seq[T]

  /**
   * Returns the set of heads.
   */
  def heads: Set[T]

  /**
   * Returns whether `x` is an head element.
   */
  def isHead(x: T): Boolean

  /**
   * Returns the sequence of elements and parenthesis representing the hierarchical ordering.
   */
  def toSeqWithParenthesis: Seq[HierarchicalOrdering.Element[T]]
  
  /**
   * Converts a hierchical ordering into a string on the basis of its parenthesized sequence
   */
  override def toString = toSeqWithParenthesis.mkString(" ")    
}

/**
 * The companion class for a hierarchical ordering contains the definition of the `Element` class and some
 * factory methods.
 */
object HierarchicalOrdering {

  /**
   * An Element[T] is either `Left` (left parenthesis), `Right` (right parenthesis) or `Val(x)` where `x` is a value of type `T`.
   * A sequence of Element is the standard representation of a hierarchical ordering.
   */
  sealed abstract class Element[+T]
  final case object Left extends Element[Nothing] {
    override def toString = "("
  }
  final case object Right extends Element[Nothing] {
    override def toString = ")"
  }
  final case class Val[T](val u: T) extends Element[T] {
    override def toString = u.toString
  }

  /**
   * Check if `seq` is a correct parenthesized sequence of elements
   */
  def validateSeqWithParenthesis[T](seq: Seq[Element[T]]): Boolean = {
    var opened = 0
    var lastopened = false
    for (s <- seq) {
      if (lastopened && !s.isInstanceOf[Val[T]]) return false
      if (s == Left) {
        opened += 1
        lastopened = true
      } else if (s == Right) {
        opened -= 1
        lastopened = false
        if (opened < 0) return false
      } else {
        lastopened = false
      }
    }
    opened == 0
  }

  /**
   * A hierarchical ordering defined by a sequence of Element.
   */
  class SequenceBasedHierarchicalOrdering[T](seq: IndexedSeq[Element[T]]) extends HierarchicalOrdering[T] {
    if (!validateSeqWithParenthesis(seq)) throw new IllegalArgumentException("Invalid sequence of elements and parenthesis")
    
    private lazy val orderingIndex: Map[T, Int] = (for { (x, i) <- seq.zipWithIndex; if x.isInstanceOf[Val[T]]; Val(u) = x } yield u -> i)(collection.breakOut)
    def toSeq = for { x <- seq.view; if x.isInstanceOf[Val[T]]; Val(u) = x } yield u
    def toSeqWithParenthesis = seq
    lazy val heads: Set[T] = (for { i <- 0 until seq.length; if seq(i) == Left; Val(u) = seq(i + 1) } yield u)(collection.breakOut)
    def isHead(x: T) = heads contains x
    def compare(x: T, y: T) = orderingIndex(x) - orderingIndex(y)
  }

  /**
   * Builds a hierarchical ordering from a sequence of `Element`s.
   */
  def apply[T](els: Element[T]*) = new SequenceBasedHierarchicalOrdering[T](els.toIndexedSeq)
}
