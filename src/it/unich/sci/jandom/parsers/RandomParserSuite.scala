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
package it.unich.sci.jandom.parsers

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import it.unich.sci.jandom.targets._

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class RandomParserSuite extends FunSuite with Checkers {
  test ("very simple program") {
    val prog: String =  """
      id <- function(x) x = 1
    """
    val parsed = SLILProgram(List("x"), List(1), AssignStmt(1,new LinearForm(List(1,0)))) 
    expect(parsed) { RandomParser.parseProgram(prog)}
  }
  
  test ("simple random program") {
    val prog: String = """
    	function xyline(x) {
          y = 0
          while (y < x) x=x+1
        }
    """
    val parsed = SLILProgram(List("x","y"), List(1), 
        CompoundStmt(List(
            AssignStmt(2,LinearForm.fromCoefficient[Int](0)),
            WhileStmt(AtomicCond(new LinearForm(List(0,-1,1)), ComparisonOperators.LT), 
                AssignStmt(1,new LinearForm(List(1,1)))
            )
        )))
   expect(parsed) { RandomParser.parseProgram(prog) }     
    }
}