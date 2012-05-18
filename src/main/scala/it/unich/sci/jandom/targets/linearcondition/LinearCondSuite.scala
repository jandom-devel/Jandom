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
package targets.linearcondition

import targets.{Environment,LinearForm}
import domains.BoxDouble
import org.scalatest.FunSuite

/**
 * Test suite for linear conditions.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class LinearCondSuite extends FunSuite {
  val env = Environment("x","y")
  val lf1 = LinearForm(List(-3,1,0),env)
  val lf2 = LinearForm(List(-6,1,0),env)
  val cond1 = AtomicCond(lf1,AtomicCond.ComparisonOperators.LTE)
  val cond2 = AtomicCond(lf2,AtomicCond.ComparisonOperators.GTE)
  val full = BoxDouble.full(env.size) 
  
  test("atomic conditions") {   
    expect (  BoxDouble(Array(Double.NegativeInfinity,Double.NegativeInfinity), Array(3,Double.PositiveInfinity)) ) { cond1.analyze(full) } 
  }	
  
  test("and/or/not conditions") {      
    expect ( BoxDouble(Array(3,Double.NegativeInfinity),Array(6,Double.PositiveInfinity)) ) { OrCond(cond1,cond2).opposite.analyze(full) }
    expect ( BoxDouble(Array(3,Double.NegativeInfinity),Array(6,Double.PositiveInfinity)) ) { NotCond(OrCond(cond1,cond2)).analyze(full) }
  }
}
