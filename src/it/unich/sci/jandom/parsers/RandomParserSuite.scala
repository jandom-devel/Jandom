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
package parsers

import targets.{Environment,LinearForm}
import targets.linearcondition._
import targets.slil._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
 * Test suite for RandomParser.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class RandomParserSuite extends FunSuite with Checkers {
  test ("very simple program") {
    val prog: String =  """
      xyz <- function(x,y) x = 1
    """    
    val env = Environment("x","y")    
    val program = SLILProgram(env,  Seq(0,1), AssignStmt(0, new LinearForm(List(1,0,0),env)))
    val parsed = RandomParser.parseProgram(prog).get
    expect(program) { parsed }
  }
  
  test ("simple random program") {
    val prog: String = """
       xyline <- function(x) {
          y = 0;
          while (y < x) 
    		y=y+1
      }
    """      
    val env = Environment("x","y")
    val program = SLILProgram(env, List(0),
        CompoundStmt(List(
            AssignStmt(1,LinearForm.fromCoefficient[Int](0,env)),
            WhileStmt(AtomicCond(new LinearForm(List(0,-1,1),env), AtomicCond.ComparisonOperators.LT), 
                AssignStmt(1,new LinearForm(List(1,0,1),env))
            )
       )))
    expect(program) { RandomParser.parseProgram(prog).get }     
  }
}
