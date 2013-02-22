/**
 * Copyright 2013 Gianluca Amato
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

package it.unich.sci.jandom
package ppfactories

import targets.MockTarget
import org.scalatest.FunSpec

/**
 * A test suite for memoizing factories.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class MemoizingFactorySuite extends FunSpec {
  
  private class NumberFactory extends PPFactory[MockTarget,Int] {
    var i: Int = 0
    def apply(pp: MockTarget#WideningPoint) = {
      i+=1
      i
    } 
  }
  
  describe("PerPP Widening Factory") {
    it ("should create differente instances of a widening for each program point")  {
      val tgt = new MockTarget()
      val dwf = MemoizingFactory(new NumberFactory,tgt)
      val i0 = dwf(0)      
      val i1 = dwf(0)
      val i2 = dwf(1)
      val i3 = dwf(0)
      expectResult(1) { i0 }
      expectResult(1) { i1 }
      expectResult(2) { i2 }
      expectResult(1) { i3 }
    }   
  }
}
