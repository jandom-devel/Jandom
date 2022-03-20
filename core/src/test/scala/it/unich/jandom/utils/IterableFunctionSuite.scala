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

package it.unich.jandom.utils

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funspec.AnyFunSpec

class IterableFunctionSuite extends AnyFunSpec with ScalaCheckPropertyChecks {
  describe("An empty iterable function") {
    val f = IterableFunction.empty[Int,Int]
    it("has empty domain") {
      assert(f.isEmpty)
    }
  }

  describe("An interable function derived from a map") {
    it("has the same functional behaviour of the map") {
      forAll { (m: Map[Int, Int]) =>
        val f: IterableFunction[Int, Int] = m
        for ( (x,y) <- m.toSeq) assertResult(y) { f(x) } 
      }        
    }
  }
}
