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
import targets.linearcondition.LinearCond

/**
 * The class for a while statement.
 * @param condition the guard of the statement
 * @param body the body of the statement
 */
case class WhileStmt(condition: LinearCond, body: SLILStmt) extends SLILStmt {
  var lastInvariant: NumericalProperty[_] = null
  var lastBodyResult: NumericalProperty[_] = null

  override def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property], ann: Annotation[Property]): Property = {
    import params.WideningScope._
    
    val widening = params.wideningFactory(this, 1)
    val narrowing = params.narrowingFactory(this, 1)
    
    var bodyResult = if (lastBodyResult != null) lastBodyResult.asInstanceOf[Property] else input.empty
    var newinvariant =  if (lastInvariant != null) lastInvariant.asInstanceOf[Property] else input.empty
    if (params.wideningScope == Random) newinvariant = newinvariant union input
    var invariant = newinvariant
    do {
      invariant = newinvariant
      params.wideningScope  match {        
        case Random =>
          bodyResult = body.analyze(condition.analyze(invariant), params, ann)
          newinvariant = widening(invariant,bodyResult)	
        case BackEdges => 
          newinvariant = input union bodyResult
          bodyResult = body.analyze(condition.analyze(input union newinvariant), params, ann)
        case Output => 
          newinvariant = widening(invariant,input union bodyResult)
          bodyResult = body.analyze(condition.analyze(newinvariant), params, ann)         
      }
      println(invariant)
      println(newinvariant)
      
    } while (newinvariant > invariant)
    if (params.narrowingStrategy == params.NarrowingStrategy.Separate)   
    	do {
    		invariant = newinvariant
    		newinvariant = narrowing(invariant, input union body.analyze(condition.analyze(invariant), params, ann))
    	} while (newinvariant < invariant)
    ann((this, 1)) = invariant
    if (params.allPPResult) ann((this, 2)) = condition.analyze(invariant)    
    return condition.opposite.analyze(invariant)
  }

  override def mkString(ann: Annotation[_], level: Int, ppspec: PrettyPrinterSpec): String = {  
    val spaces = ppspec.indent(level)
    spaces + "while (" + condition + ")" +""+
      (if (ann contains (this, 1)) " " + ppspec.decorator(ann(this, 1)) else "") + " {\n" +
      (if (ann contains (this, 2)) ppspec.indent(level+1) + ppspec.decorator(ann(this, 2)) + "\n" else "") +
      body.mkString(ann, level+1, ppspec) + '\n' +
      spaces + '}'
  }
}
