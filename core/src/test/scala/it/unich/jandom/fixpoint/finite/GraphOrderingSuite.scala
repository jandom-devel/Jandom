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
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.fixpoint.finite

import org.scalatest.FunSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

class GraphOrderingSuite extends FunSpec with PropertyChecks {  
 
  describe("A trivial graph ordering") {
    it("returns the original sequence") {
      forAll { (s: Set[Int]) =>
        val seq = s.toSeq
        val o = GraphOrdering(seq: _*)
        assertResult(seq)(o.toSeq)
      }
    }
    it("respects the order of the input sequence") {
      forAll { (s: Set[Int]) =>
        whenever(s.size > 0) {
          val seq = s.toSeq
          val o = GraphOrdering(seq: _*)
          val g = Gen.choose(0, s.size - 1)
          forAll(g, g) { (x: Int, y: Int) =>
            assertResult(scala.math.signum(x compare y))(scala.math.signum(o.compare(seq(x), seq(y))))
          }
        }
      }
    }
    it("has only head elements") {
      forAll { (s: Set[Int]) =>
        val seq = s.toSeq
        val o = GraphOrdering(seq: _*)
        for (x <- s) assert(o.isHead(x))
        assertResult(s)(o.heads)
      }
    }
  }
}
