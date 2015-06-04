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

import it.unich.jandom.fixpoint._
import it.unich.jandom.utils.Relation

/**
 * A weak topological ordering for a graph is a hierarchical ordering which respects the structure of the graph
 * according to Bourdoncle's paper "Efficient chaotic iteration strategies with widenings", FMPA'93.
 * Each WFO is strictly connected to a relation.
 * @tparam N the type of the nodes of the graph
 */
trait WTOrdering[N] extends HierarchicalOrdering[N] {
  /**
   * The relation/graph this WTO is related to.
   */
  val relation: Relation[N, N]
}

object WTOrdering {
  import HierarchicalOrdering._  
  
  /**
   * Returns a WTO from a DFO. Components are opened for each head, and they are all closed at the end.
   */
  def apply[N](o: DFOrdering[N]) = new WTOrdering[N] {      
    val relation = o.relation
    def toSeq = o.toSeq
    def isHead(x: N) = o.isHead(x)
    def heads = o.heads
    def compare(x: N, y: N) = o.compare(x, y)
    lazy val toSeqWithParenthesis = {
      val buffer = collection.mutable.Buffer.empty[Element[N]]
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
