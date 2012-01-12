/**
 * This it the unit test for Box
 *
 * Copyright 2011 Gianluca Amato
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

package it.unich.sci.jandom.domains

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers 

/**
 * This is a unit test for the BoxDouble numerical domain.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class BoxSuite extends FunSuite with Checkers {
    
    test("constructors should only work with normalized bounds")  {
      intercept[IllegalArgumentException] { BoxDouble(Array(0,2),Array(0,2,3)) }
      intercept[IllegalArgumentException] { BoxDouble(Array(Double.PositiveInfinity,2),Array(0,2,3)) }
    }
           
    test("operations on boxes") {
    	val i = BoxDouble(Array(1,2),Array(5,4))
        val j = BoxDouble(Array(0,3),Array(3,4))        
        expect(i union j) { BoxDouble(Array(0,2),Array(5,4)) }     
    	expect(i intersection j) { BoxDouble(Array(1,3),Array(3,4))}    	
    } 
            
    test("empty boxes") {
      val i = BoxDouble(Array(-1,-2),Array(-4,3))
      val j = BoxDouble(Array(0,0),Array(5,5))
      expect(i) { BoxDouble.empty(2) }
      expect(i union j) { j }
      expect(i intersection j) { i }
      expect(i.linearAssignment(1,Array(1,1),1)) { i }
      expect(i.linearAssignment(1,Array(0,0),0)) { i }
    }         
}
