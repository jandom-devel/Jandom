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

package it.unich.jandom.fixpoint

import org.scalatest.prop.PropertyChecks
import org.scalatest.FunSpec
import org.scalacheck.Gen

class PartialUFactorySuite extends FunSpec with PropertyChecks {
  describe("An empty factory") {
    val f = PartialUFactory.empty[Int, Int]
    it("has empty domain") {
      assert(f.domain.isEmpty)
    }
  }

  describe("A map derived factory") {
    it("has the same functional behaviour of  map") {
      forAll { (m: Map[Int, Int]) =>
        val f: PartialUFactory[Int, Int] = m
        assertResult(m.keySet) { f.domain }        
        for ( (x,y) <- m.toSeq) assertResult(y) { f(x) } 
      }        
    }
  }
}
