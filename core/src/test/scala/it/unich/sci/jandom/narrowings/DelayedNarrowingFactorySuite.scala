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
package narrowings

import targets.MockTarget
import domains.BoxDouble
import org.scalatest.FunSpec
import it.unich.sci.jandom.ppfactories.DelayedNarrowingFactory

/**
 * Test suite for delayed narrowing factories.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class DelayedNarrowingFactorySuite extends FunSpec {  
  
  describe("Delayed Narrowing Factory") {
    it ("should create a difference instance of a delayed narrowing each time it is called")  {      
      val dwf = DelayedNarrowingFactory[MockTarget#ProgramPoint](DefaultNarrowing,1)
      val wd = dwf(0)       
      val d1 = BoxDouble(Array(0),Array(3))
      val d2 = BoxDouble(Array(1),Array(3))
      val d3 = wd(d1,d2)    
      expectResult ( BoxDouble(Array(1),Array(3)) ) { d3 }
      val wd2 = dwf(0)
      val d4 = BoxDouble(Array(2),Array(3))
      val d5 = wd(d3,d4)
      expectResult ( BoxDouble(Array(1),Array(3)) ) { d5 }
      val wd3 = dwf(1)
      val d6 = wd3(d3,d4)
      expectResult ( BoxDouble(Array(2),Array(3)) ) { d6 }          
    }
    
    it ("should reject negative delays") {
      intercept[IllegalArgumentException] { DelayedNarrowingFactory(DefaultNarrowing,-1) }
    }
  }      
}
