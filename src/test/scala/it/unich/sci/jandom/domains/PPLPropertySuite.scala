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
package domains

import parma_polyhedra_library.Octagonal_Shape_double
import org.scalatest.FunSuite

/**
 * Test suite for the PPLProperty numerical domain.
 * @author Gianluca Amato <g.amato@unich.it>
 */
class PPLPropertySuite extends FunSuite {
  val octDomain = new PPLDomain[Octagonal_Shape_double]
  val full = octDomain.full(3)
  val empty = octDomain.empty(3)
 
  test ("full should be full") { 
    expect ( true ) { full.isFull }
  }
  
  test ("full should not be empty") {  
    expect ( false ) { full.isEmpty }  
  }
  
  test ("empty should be empty") {    
    expect ( true ) { empty.isEmpty }
  }
  
  test ("empty should not be full") {
	expect ( false ) { empty.isFull }  
  }

  test ("empty should be strictly less than full") {
    expect ( true ) { empty < full }
    expect ( true ) { empty <= full }
  }

  test ("various operations") {
    val obj = full.linearAssignment(0, Array(0,0,0), 0)
    val obj2 = full.linearAssignment(1, Array(0,0,0), 0)
    val obj3 = full.linearAssignment(2, Array(0,0,0), 0)
    val obj4 = full.linearAssignment(2, Array(0,0,0), 1)
    val obj5 = obj4 union obj3
    expect (true) { obj5 > obj4 }
    val obj7 = obj5.linearInequality(Array(0,0,1), 1)
    expect (empty) { obj7 }   
    val obj8 = obj4 widening obj3
    expect (obj5) { obj8 }
  }
}
