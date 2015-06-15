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

import it.unich.jandom.utils.Relation
import scala.collection.immutable.ListSet
import scala.annotation.tailrec

/**
 * This trait represents a depth-first ordering of a graph, as it appears in Aho, Sehti, Ullman book
 * on compilers. It extends the concept of graph ordering distinguishing between Advancing, Retreating
 * and Cross edges.
 * @tparam N the type of the nodes of the graph
 */
trait DFOrdering[N] extends GraphOrdering[N] {
  import DFOrdering.EdgeType._
  
  /**
   * It returns the type of an edge u->v. 
   * @param u source node
   * @param v target node
   */
  def edgeType(u: N, v: N): EdgeType
}

/**
 * The companion class for a DFOrdering defines the required enumerations and factory
 * methods.
 */
object DFOrdering {
  /**
   * Every edge may be of three different kinds: Advancing, Retreating and Cross.
   */
  object EdgeType extends Enumeration {
    val Advancing = Value
    val Retreating = Value
    val Cross = Value
    type EdgeType = Value
  }

  /**
   * Returns a DFOrdering for the graph encoded by relation `r` and starting nodes in `entries`.
   */
  def apply[N](r: Relation[N, N], entries: Iterable[N]) = new DFOrderingFromR[N](r, entries)
  
  /**
   * Returns a DFOrdering for the graph encoded by relation `r` and starting nodes in `entries`.
   */
  def apply[N](r: Relation[N, N])(entries: N*) = new DFOrderingFromR[N](r, entries)

  /**
   * This class is a depth-first ordering for the relation `r`. Note that this ordering only contains elements reachable from entries.
   * For all other values of type `N`, the result of using the ordering is unspecified.
   * @param relation the relation from which compute the DFOrdering.
   * @param entries nodes from which to start the visit.
   */
  final class DFOrderingFromR[N](relation: Relation[N, N], entries: Iterable[N]) extends DFOrdering[N] {
    import DFOrdering.EdgeType._

    private val dfn = collection.mutable.HashMap.empty[N, Int]
    private val dfst = collection.mutable.Set.empty[(N, N)]
    initDFO()

    def initDFO() {
      val visited = collection.mutable.Set.empty[N]
      var c = 0
      def dfsVisit(u: N) {
        visited += u
        for (v <- relation.image(u); if !(visited contains v)) {
          dfst += (u -> v)
          dfsVisit(v)
        }
        dfn += u -> c
        c -= 1
      }
      for (x <- entries) if (! (visited contains x)) dfsVisit(x)
      
      // add nodes not reachable from entries
      for (x <- relation.domain) if (! (visited contains x)) dfsVisit(x)
      for (x <- relation.codomain) if (! (visited contains x)) dfsVisit(x)
    }

    def toSeq = (relation.domain union relation.inverse.domain).toSeq.sorted(this)

    def compare(x: N, y: N) = scala.math.signum(dfn(x) - dfn(y))

    /**
     * Returns whether y is a child of x in the depth-first spanning tree.
     */
    @tailrec private def connected(x: N, y: N): Boolean = {
      val z = dfst.find(_._2 == y)
      if (z.isEmpty)
        false
      else if (z.get._1 == x)
        true
      else
        connected(x, z.get._1)
    }

    def edgeType(x: N, y: N) = if (y <= x)
      Retreating
    else if (connected(x,y))
      Advancing
    else 
      Cross

    def isHead(u: N) = relation.inverse.image(u).exists { v => u <= v }

    def heads = for ((u, v) <- relation.graph; if edgeType(u, v) == Retreating) yield v
  }
}
