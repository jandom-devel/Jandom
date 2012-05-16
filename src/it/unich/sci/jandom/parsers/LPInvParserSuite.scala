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
package parsers


import targets.{Environment,LinearForm}
import targets.linearcondition._
import targets.LinearAssignment
import targets.lts._
import org.scalatest.FunSuite

/**
 * Test suite for RandomParser.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class LPInvParserSuite extends FunSuite  {
   test("simple LTS") {
    val env = targets.Environment("x")
	val l1 = Location("start", Nil)
	val l2 = Location("ciclo", List(FalseCond))
	val t1 = Transition("init", l1, l2, 
	    guard = Nil, 
	    assignments = List(LinearAssignment(0,LinearForm.fromCoefficient(0,env))))
	val t2 = Transition("loop", l2, l2, 
	    guard = List(AtomicCond(LinearForm(List(-10,1),env), AtomicCond.ComparisonOperators.LTE)),
	    assignments = List(LinearAssignment(0,LinearForm(List(1,1),env))))
	val lts = LTS(IndexedSeq(l1,l2), Seq(t1,t2), env)
		
	val ltsString = """
	  var x;
	  location start with ( );
	  location ciclo with ( FALSE );
	  transition init start -> ciclo with Guard ()
	    x := 0;
	  transition loop ciclo -> ciclo with Guard ( x<=10 )
	    x := x+1;	    	  
	  end
	  """
	  expect(lts) { LPInvParser.parseProgram(ltsString).get } 	 
    }   
}
