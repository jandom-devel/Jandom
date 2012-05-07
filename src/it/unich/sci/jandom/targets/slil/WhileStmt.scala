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

import domains.NumericalProperty
import targets.Parameters
import targets.linearcondition.LinearCond
import annotations.BlackBoard
import widenings.Widening

/**
 * The class for a while statement.
 * @param condition the guard of the statement
 * @param body the body of the statement 
 */
case class WhileStmt(condition: LinearCond, body: SLILStmt) extends SLILStmt {  
  var savedInvariant : NumericalProperty[_] = null
  var savedFirst: NumericalProperty[_] = null
  private var widening: Widening = null
 
  override def analyze[Property <: NumericalProperty[Property]] (input: Property, params: Parameters[Property,SLILProgram], ann: BlackBoard[SLILProgram]): Property =  {    
    var newinvariant = input
    var invariant = input
    if (widening==null) widening = params.wideningFactory.widening
    do {      
      invariant = newinvariant
      newinvariant = widening(invariant, input union body.analyze(condition.analyze(invariant), params, ann))
    } while (newinvariant > invariant)          
    do {
      invariant = newinvariant
      newinvariant = params.narrowing[SLILProgram](invariant, input union body.analyze(condition.analyze(invariant),params, ann), ann, this.hashCode)      
    } while (newinvariant < invariant)    
    savedInvariant = invariant
    savedFirst = condition.analyze(invariant)
    return condition.opposite.analyze(invariant)
  }  

  override def formatString(indent: Int, indentSize: Int) = {
    val spaces = " "*indentSize*indent
    spaces + "while (" + condition +")"  + 
      (if (savedInvariant!=null) " "+savedInvariant else "") + " {\n" +
      spaces + " "*indentSize + savedFirst + "\n" +
      body.formatString(indent+1, indentSize) + '\n' + 
    spaces + '}'
  }
}
