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
package targets.slil

import domains.{BoxDouble,NumericalPropertyAnnotation}
import targets.LinearForm
import targets.linearcondition.AtomicCond
import org.scalatest.FunSuite

/**
 * Test suite for SLIL programs.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class SLILProgramSuite extends FunSuite {
  test ("simple program 1") {
    val env = targets.Environment("x")
    val program = SLILProgram(env, Seq(1),
        CompoundStmt(Seq(
            AssignStmt(0,LinearForm.fromCoefficient(0,env)),
            WhileStmt(AtomicCond(LinearForm(List(-10,1),env), AtomicCond.ComparisonOperators.LT), 
                AssignStmt(0,LinearForm(List(1,1),env))
            )
       )))      
    val params = new targets.Parameters(BoxDouble,program)
    val bb = new annotations.BlackBoard(program)
    program.analyze(params, bb)
    expect ( BoxDouble(Array(10), Array(11)) ) { bb(NumericalPropertyAnnotation)(program,2) }
  }  
}
