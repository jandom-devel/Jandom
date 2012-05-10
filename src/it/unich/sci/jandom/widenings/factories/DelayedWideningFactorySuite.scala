/**
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
 *
 * (c) 2012 Gianluca Amato
 */

package it.unich.sci.jandom
package widenings.factories

import targets.MockTarget
import domains.BoxDouble
import org.scalatest.FunSpec

/**
 * Test suite for delayed widening factories.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class DelayedWideningFactorySuite extends FunSpec {  
  
  describe("Delayed Widening Factory") {
    it ("should create a difference instance of a delayed widening each time it is called")  {      
      val dwf = DelayedWideningFactory[MockTarget](DefaultWideningFactory,1)
      val wd = dwf(0)       
      val d1 = BoxDouble(Array(0),Array(1))
      val d2 = BoxDouble(Array(1),Array(2))
      val d3 = wd(d1,d2)    
      expect ( BoxDouble(Array(0),Array(2)) ) { d3 }
      val wd2 = dwf(0)
      val d4 = BoxDouble(Array(2),Array(3))
      val d5 = wd2(d3,d4)
      expect ( BoxDouble(Array(0),Array(3)) ) { d5 }
      val wd3 = dwf(1)
      val d6 = BoxDouble(Array(3),Array(4))
      val d7 = wd3(d5,d6)
      expect ( BoxDouble(Array(0),Array(4)) ) { d7 }          
    }
    
    it ("should reject negative delays") {
      intercept[IllegalArgumentException] { DelayedWideningFactory(DefaultWideningFactory,-1) }
    }
  }      
}
