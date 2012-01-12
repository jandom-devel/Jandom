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
 * (c) 2011 Gianluca Amato
 */
package it.unich.sci.jandom.targets

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class LinearFormSuite extends FunSuite with Checkers {
    
  test("standard constructor") {
    val lf = LinearForm(List(1,2,-1))
    expect("1+2*v1-v2") { lf.toString }
    val lf2 = LinearForm(List(1,0,3))
    expect("1+3*v2") { lf2.toString }
  }	
  
  test("fromVar constructor") {
    var lf = LinearForm.fromVar[Int](3)
    expect("v3") { lf.toString }
  }
  
  test("from coefficient and var") {
    val lf1 = LinearForm(List(0,0,2))
    val lf2 = LinearForm.fromCoefficientVar(2,2)
    expect(lf1) { lf2 }
  }
  
  test("sum") {
    val lf1 = LinearForm(List(1,2,-1))
    val lf2 = LinearForm(List(1,0,3))
    val lf3 = LinearForm(List(2,2,2))
  	expect(lf3) { lf1+lf2 }    
  }
  

}