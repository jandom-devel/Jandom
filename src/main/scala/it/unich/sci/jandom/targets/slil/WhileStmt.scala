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
  import AnalysisPhase._
  
  var lastInvariant: NumericalProperty[_] = null
  var lastBodyResult: NumericalProperty[_] = null

  override def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property], 
      phase: AnalysisPhase, ann: Annotation[Property]): Property = {
    import targets.WideningScope._
    import targets.NarrowingStrategy._
    
    // Determines widening operators to use
    val widening = params.wideningFactory(this, 1)
    val narrowing = params.narrowingFactory(this, 1)
    
    // Determines initial value of the analysis, depending on the current phase
    var bodyResult = if (lastBodyResult != null && phase != AscendingRestart) lastBodyResult.asInstanceOf[Property] else input.empty
    var newinvariant = if (lastInvariant != null && phase != AscendingRestart) lastInvariant.asInstanceOf[Property] else input.empty
    var invariant = newinvariant

    // If needed, perform ascending phase. If in AscendingRestart, perform first step in AscendingRestart,
    // and later steps in Ascending
    if (phase == Ascending || phase == AscendingRestart) do {
      if (params.wideningScope == Random) newinvariant = newinvariant union input
      var currentPhase = phase
      invariant = newinvariant
      params.wideningScope  match {        
        case Random =>
          bodyResult = body.analyze(condition.analyze(invariant), params, currentPhase, ann)
          newinvariant = widening(invariant,bodyResult)	
        case BackEdges => 
          newinvariant = input union bodyResult
          bodyResult = body.analyze(condition.analyze(input union newinvariant), params, currentPhase, ann)
        case Output => 
          newinvariant = widening(invariant,input union bodyResult)
          bodyResult = body.analyze(condition.analyze(newinvariant), params, currentPhase, ann)         
      }
      currentPhase = Ascending
    } while (newinvariant > invariant)
      
    // If needed, perform descending step
    if (phase == Descending || params.narrowingStrategy == Restart || params.narrowingStrategy == Continue ) {
      val newphase = if (params.narrowingStrategy == Restart) AscendingRestart else Descending 
      do {
    	invariant = newinvariant
    	newinvariant = narrowing(invariant, input union body.analyze(condition.analyze(invariant), params, newphase, ann))
      } while (newinvariant < invariant)
    }
    ann((this, 1)) = invariant
    lastInvariant = invariant
    lastBodyResult = bodyResult
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
