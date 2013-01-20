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

package it.unich.sci.jandom
package targets

import org.scalatest.FunSuite

/** 
 * The Test Suite for the LinearForm class
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class LinearFormSuite extends FunSuite {
  
  val env = Environment("v1","v2")
    
  test("LinearForm companion object standard constructor") {
    val lf = LinearForm(List(1,2,-1),env)
    expectResult("1+2*v1-v2") { lf.toString }
    val lf2 = LinearForm(List(1,0,3),env)
    expectResult("1+3*v2") { lf2.toString }
  }	
  
  test("LinearForm companion object fromVar constructor") {
    var lf = LinearForm.fromVar[Int](2,env)
    expectResult("v2") { lf.toString }
  }
  
  test("LinearForm companion object fromCoefficientVar constructor") {
    val lf1 = LinearForm(List(0,0,2),env)
    val lf2 = LinearForm.fromCoefficientVar(2,2,env)
    expectResult(lf1) { lf2 }
  }
  
  test("LinearForm companion addition") {
    val lf1 = LinearForm(List(1,2,-1),env)
    val lf2 = LinearForm(List(1,0,3),env)
    val lf3 = LinearForm(List(2,2,2),env)
  	expectResult(lf3) { lf1+lf2 }    
  }  
  
  test("Equality") {
    val env2 = Environment("v1","v2")
    val lf1 = LinearForm(Seq(1,0,3),env)
    val lf2 = LinearForm(IndexedSeq(1,0,3),env)
    expectResult(lf1) { lf2 }
    val lf3 = LinearForm(Seq(1,0,1), env)
    expectResult(false) (lf1 == lf3)
    val lf4 = LinearForm(Seq(1,0,3), Environment("v1","v4"))
    expectResult(false) (lf1 == lf4)
  }
}