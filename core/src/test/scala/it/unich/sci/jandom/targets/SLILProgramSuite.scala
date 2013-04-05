/**
 * Copyright 2013 Gianluca Amato
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
package targets

import domains.BoxDouble
import linearcondition.AtomicCond
import slil._
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
            AssignStmt(0,LinearForm.fromCoefficient(0)),
            WhileStmt(AtomicCond(LinearForm(List(-10,1)), AtomicCond.ComparisonOperators.LT), 
                AssignStmt(0,LinearForm(List(1,1)))
            )
       )))      
    val params = new Parameters(program: SLILStmt) { val domain = BoxDouble }
    val ann = program.analyze(params)
    expectResult ( BoxDouble(Array(10), Array(11)) ) { ann(program,2) }
  } 
  
  test("input vs output widening") {
    val source="""
      localwidening = function() {
    		i = 0
    		while (TRUE) {
              tag(0)
    		  if (brandom()) 
    		     i = 1
    		  else
    		     i = -1
            }
      }     
    """
    val parsed = parsers.RandomParser().parseProgram(source)
    val program = parsed.get
    
    val params = new Parameters(program: SLILStmt) { val domain = domains.BoxDouble }
    params.narrowingStrategy = NarrowingStrategy.None
    params.wideningScope = WideningScope.Output
    program.analyze(params)
    expectResult ( BoxDouble.full(1) ) { params.tag(0) }
    
    params.narrowingStrategy = NarrowingStrategy.None
    params.wideningScope = WideningScope.Random
    program.analyze(params)
    expectResult ( BoxDouble.full(1) ) { params.tag(0) }
    
    params.narrowingStrategy = NarrowingStrategy.None
    params.wideningScope = WideningScope.BackEdges
    program.analyze(params)
    expectResult ( BoxDouble(Array(-1), Array(1)) ) { params.tag(0) }
  }
}
