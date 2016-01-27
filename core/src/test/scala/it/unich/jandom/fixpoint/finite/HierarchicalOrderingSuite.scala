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

import HierarchicalOrdering._
import org.scalatest.FunSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableFor2
import it.unich.jandom.utils.Relation
import scala.collection.immutable.ListSet

class IterativeStrategySuite extends FunSpec with TableDrivenPropertyChecks {

  val seqp1 = Seq(Left, Val(0), Left, Val(1), Val(2), Val(3), Right, Right)
  val seq1 = Seq(0, 1, 2, 3)
  val heads1 = Set(0, 1)
  val o1 = HierarchicalOrdering[Int](seqp1: _*)
  val out1 = "( 0 ( 1 2 3 ) )"

  val seqp2 = Seq(Left, Val(3), Left, Val(2), Val(1), Right, Val(0), Right)
  val seq2 = Seq(3, 2, 1, 0)
  val heads2 = Set(3, 2)
  val o2 = HierarchicalOrdering[Int](seqp2: _*)
  val out2 = "( 3 ( 2 1 ) 0 )"

  val graph = Relation[Int, Int](ListSet(1 -> 3, 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 6, 4 -> 5, 4 -> 8, 5 -> 7, 6 -> 7, 7 -> 4, 7 -> 8, 8 -> 4, 8 -> 3, 8 -> 10, 8 -> 9, 9 -> 1, 10 -> 7))
  val dfo = DFOrdering(graph)(1)
  val o3 = HierarchicalOrdering(dfo)
  val heads3 = dfo.heads
  val seq3 = dfo.toSeq
  val seqp3 = Seq(Left, Val(1), Val(2), Left, Val(3), Left, Val(4), Val(5), Val(6), Left, Val(7), Val(8), Val(9), Val(10), Right, Right, Right, Right)
  val out3 = "( 1 2 ( 3 ( 4 5 6 ( 7 8 9 10 ) ) ) )"

  val table = Table(("Ordering", "Parenthesized Sequence", "Sequence", "Heads", "Output"),
    (o1, seqp1, seq1, heads1, out1),
    (o2, seqp2, seq2, heads2, out2),
    (o3, seqp3, seq3, heads3, out3))
    
  val wto = Table(("Relation", "WTO"), (graph, o3))

  describe("The sequence based factory methods") {
    it("generates excpetions on non-well parenthesized sequences") {
      intercept[IllegalArgumentException] { HierarchicalOrdering[Int](Left, Val(0)) }
      intercept[IllegalArgumentException] { HierarchicalOrdering[Int](Left, Val(0), Right, Right) }
      intercept[IllegalArgumentException] { HierarchicalOrdering[Int](Left, Left, Val(0), Right, Right) }
    }
  }

  describe("A hierarchical ordering") {
    it("may be converted correctly to sequence with or without parenthesis") {
      forAll(table) { (o, seqp, seq, heads, out) =>
        assertResult(seq) { o.toSeq }
        assertResult(seqp) { o.toSeqWithParenthesis }
      }
    }

    it("has the correct heads") {
      forAll(table) { (o, seqp, seq, heads, out) =>
        assertResult(heads) { o.heads }
        for (x <- o.toSeq) assertResult(o.heads contains x) { o.isHead(x) }
      }
    }

    it("implements the correct ordering") {
      forAll(table) { (o, seqp, seq, heads, out) =>
        for (x <- o.toSeq; y <- o.toSeq) {
          assertResult(scala.math.signum(o.toSeq.indexOf(x) compare o.toSeq.indexOf(y))) { scala.math.signum(o.compare(x, y)) }
        }
      }
    }
    
    it("is correctly converted into a string") {
      forAll(table) { (o, seqp, seq, heads, out) =>
        assertResult (out) { o.toString }
      }
    }
  }

  describe("A weak topological ordering") {
    it("has all back edges pointing to heads") {
      forAll(wto) { (r,o) =>
        r.graph.forall { case (u, v) => o.lt(u, v) || o.isHead(u) }
      }
    }
  }
}
