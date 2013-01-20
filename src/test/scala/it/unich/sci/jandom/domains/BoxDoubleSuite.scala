/**
 * This it the unit test for the BoxDouble numerical domain.
 *
 * Copyright 2011,2012 Gianluca Amato
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
package domains

import org.scalatest.FunSuite

/**
 * This is a unit test for the BoxDouble numerical domain.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class BoxDoubleSuite extends FunSuite {
    
    test("constructors should only work with normalized bounds")  {
      intercept[IllegalArgumentException] { BoxDouble(Array(0,2),Array(0,2,3)) }
      intercept[IllegalArgumentException] { BoxDouble(Array(Double.PositiveInfinity,2),Array(0,2,3)) }
    }
           
    test("operations on boxes") {
    	val i = BoxDouble(Array(1,2),Array(5,4))
        val j = BoxDouble(Array(0,3),Array(3,4))        
        expectResult(i union j) { BoxDouble(Array(0,2),Array(5,4)) }     
    	expectResult(i intersection j) { BoxDouble(Array(1,3),Array(3,4))}    	
    } 
            
    test("empty boxes") {
      val i = BoxDouble(Array(-1,-2),Array(-4,3))
      val j = BoxDouble(Array(0,0),Array(5,5))
      expectResult(i) { BoxDouble.empty(2) }
      expectResult(i union j) { j }
      expectResult(i intersection j) { i }
      expectResult(i.linearAssignment(1,Array(1,1),1)) { i }
      expectResult(i.linearAssignment(1,Array(0,0),0)) { i }
    }
    
    test("linear inequations") {
      val i = BoxDouble.full(2).linearInequality(Array(1,0),-3)
      val j = BoxDouble(Array(0,0),Array(5,5)).linearInequality(Array(1,1),-4)
      expectResult(BoxDouble(Array(Double.NegativeInfinity,Double.NegativeInfinity), Array(3,Double.PositiveInfinity))) { i }
      expectResult(BoxDouble(Array(0,0),Array(4,4))) { j }
    }
    
    test("string conversion") {
      val i = BoxDouble(Array(0,-1), Array(2,3))
      expectResult(Seq("0.0 <= x <= 2.0","-1.0 <= y <= 3.0")) { i.mkString(IndexedSeq("x","y")) }
      expectResult("[ 0.0 <= v0 <= 2.0 , -1.0 <= v1 <= 3.0 ]") { i.toString }
    }
}
