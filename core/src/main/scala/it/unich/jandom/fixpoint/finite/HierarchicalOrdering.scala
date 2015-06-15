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
 */
trait HierarchicalOrdering[N] extends GraphOrdering[N] {
  import HierarchicalOrdering._
  
  /**
   * A sequence of elements and parenthesis representing the hierarchical ordering.
   */
  def toSeqWithParenthesis: Seq[HOElement[N]]

  /**
   * Converts a hierarchical ordering into a string on the basis of its parenthesized sequence
   */
  override def toString = toSeqWithParenthesis.mkString(" ")
}

/**
 * The companion class for a hierarchical ordering contains the definition of the `Element` class and some
 * factory methods.
 */
object HierarchicalOrdering {

  /**
   * An HOElement[N] is either `Left` (left parenthesis), `Right` (right parenthesis) or `Val(x)` where `x` is a value of type `N`.
   * A sequence of HOElements is the standard representation of a hierarchical ordering.
   */
  sealed abstract class HOElement[+N]
  final case object Left extends HOElement[Nothing] {
    override def toString = "("
  }
  final case object Right extends HOElement[Nothing] {
    override def toString = ")"
  }
  final case class Val[N](val u: N) extends HOElement[N] {
    override def toString = u.toString
  }

  /**
   * Check if `seq` is a correct parenthesized sequence of elements.
   * @param seq a sequence of HOElements.
   */
  def validateSeqWithParenthesis[N](seq: Seq[HOElement[N]]): Boolean = {
    var opened = 0
    var lastopened = false
    for (s <- seq) {
      if (lastopened && !s.isInstanceOf[Val[N]]) return false
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
   * Builds a hierarchical ordering from a sequence of HOElements.
   */
  def apply[N](els: HOElement[N]*) = new SequenceBasedHierarchicalOrdering(els.toIndexedSeq)

  /**
   * Builds a hierarchical ordering from a graph ordering. Components are opened for each head, and they are all closed at the end.
   * If the `o` is the DFO for a graph, the result is a weak-topological ordering for the same graph.
   */
  def apply[N](o: GraphOrdering[N]) = new GraphOrderingBasedHO(o)

  /**
   * A hierarchical ordering defined by a sequence of HOElements.
   */
  final class SequenceBasedHierarchicalOrdering[N](seq: IndexedSeq[HOElement[N]]) extends HierarchicalOrdering[N] {
    if (!validateSeqWithParenthesis(seq)) throw new IllegalArgumentException("Invalid sequence of elements and parenthesis")

    private lazy val orderingIndex: Map[N, Int] = (for { (x, i) <- seq.zipWithIndex; if x.isInstanceOf[Val[N]]; Val(u) = x } yield u -> i)(collection.breakOut)
    def toSeq = for { x <- seq.view; if x.isInstanceOf[Val[N]]; Val(u) = x } yield u
    def toSeqWithParenthesis = seq
    lazy val heads: Set[N] = (for { i <- 0 until seq.length; if seq(i) == Left; Val(u) = seq(i + 1) } yield u)(collection.breakOut)
    def isHead(x: N) = heads contains x
    def compare(x: N, y: N) = orderingIndex(x) - orderingIndex(y)
  }

  /**
   * A hierarchical ordering specified by a GraphOrdering. Components are opened for each head, and they are all closed at the end.
   * If the `o` is the DFO for a graph, the result is a weak-topological ordering for the same graph.
   */
  final class GraphOrderingBasedHO[N](o: GraphOrdering[N]) extends HierarchicalOrdering[N] {
    def toSeq = o.toSeq
    def isHead(x: N) = o.isHead(x)
    def heads = o.heads
    def compare(x: N, y: N) = o.compare(x, y)
    lazy val toSeqWithParenthesis = {
      val buffer = collection.mutable.Buffer.empty[HOElement[N]]
      var open = 0
      for (x <- o.toSeq) {
        if (o.isHead(x)) {
          buffer.append(Left)
          open += 1
        }
        buffer.append(Val(x))
      }
      for (_ <- 0 until open) { buffer.append(Right) }
      buffer.toSeq
    }
  }

}
