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

package it.unich.sci.jandom.targets.slil

import it.unich.sci.jandom.domains.NumericalProperty
import it.unich.sci.jandom.targets.linearcondition.LinearCond
import it.unich.sci.jandom.targets.Annotation

/**
 * The class for a while statement.
 * @param condition the guard of the statement
 * @param body the body of the statement
 */
case class WhileStmt(condition: LinearCond, body: SLILStmt) extends SLILStmt {
  import AnalysisPhase._

  /**
   * This variable keeps the last value for the invariant computed during the analysis.
   */
  var lastInvariant: NumericalProperty[_] = null

  /**
   * This variable keeps the last result for the analysis of the body of the While statement.
   */
  var lastBodyResult: NumericalProperty[_] = null

  override def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[ProgramPoint,params.Property]): params.Property = {
    import it.unich.sci.jandom.targets.WideningScope._
    import it.unich.sci.jandom.targets.NarrowingStrategy._

    // Increase nesting level since we are entering a loop
    params.nestingLevel += 1

    // Determines widening/narrowing operators to use
    val widening = params.wideningFactory(this, 1)
    val narrowing = params.narrowingFactory(this, 1)

    // Determines initial values for the analysis, depending on the calling phase
    var (bodyResult, invariant) =
      if (lastBodyResult != null && phase != AscendingRestart)
        (lastBodyResult.asInstanceOf[params.Property], lastInvariant.asInstanceOf[params.Property])
      else
        (input.empty, input.empty)

    // Keep the current phase in the variable currentPhase, and initialize
    // with the input parameter
    var currentPhase = phase

    // Declare a variable for the loop
    var newinvariant = invariant

    if (currentPhase == Ascending || currentPhase == AscendingRestart) {
      // Debug
      params.log("Beginning Ascending Chain\n")
      params.log(s"Starting Invariant: $invariant\n")
      params.log(s"Input: $input\n")

      // Initialization phase: compute the effect of entering the while node from the
      // outer cycle.
      params.wideningScope match {
        case Random => newinvariant = invariant union input
        case Output => newinvariant = invariant widening (input union bodyResult)
        case BackEdges => newinvariant = bodyResult union input
      }

      // Debug
      params.log(s"Entering Invariant: $newinvariant\n")

      do {
        invariant = newinvariant
        params.wideningScope match {
          case Random =>
            bodyResult = body.analyzeStmt(params)(condition.analyze(invariant), currentPhase, ann)
            newinvariant = widening(invariant, bodyResult)
          case BackEdges =>
            bodyResult = bodyResult widening body.analyzeStmt(params)(condition.analyze(input union newinvariant), currentPhase, ann)
            newinvariant = bodyResult union input
          case Output =>
            bodyResult = body.analyzeStmt(params)(condition.analyze(newinvariant), currentPhase, ann)
            newinvariant = widening(invariant, input union bodyResult)
        }
        // If we were in AscendingRestart phase, move to Ascending phase
        currentPhase = Ascending

        // Debug     
        params.log(s"Body Result: $bodyResult\n")
        params.log(s"Invariant: $newinvariant\n")

      } while (newinvariant > invariant)

      // Debug
      params.log(s"Final ascending invariant: $invariant\n")

      // If the strategy wants intertwined ascending and descending chains,
      // move phase to Descending
      if (params.narrowingStrategy == Restart || params.narrowingStrategy == Continue)
        currentPhase = Descending
    }

    if (currentPhase == Descending) {
      // Debug
      params.log("Beginning Descending Chain\n")
      params.log(s"Starting Invariant: $invariant\n")
      params.log(s"Starting Body Result: $bodyResult\n")
      params.log(s"Input: $input\n")
      
      
      // For narrowing, we only consider output scope
      newinvariant = narrowing(invariant, input union bodyResult)
            
      // Debug
      params.log(s"Entering Invariant: $newinvariant\n")
      
      // Determines the phase for the inner loops
      val newphase = if (params.narrowingStrategy == Restart) AscendingRestart else Descending
      do {
        invariant = newinvariant
        
        bodyResult = body.analyzeStmt(params)(condition.analyze(invariant), newphase, ann)
        newinvariant = invariant narrowing (input union bodyResult)

        // Debug
        params.log(s"Body Result: $bodyResult\n")
        params.log(s"Invariant: $newinvariant\n")
      } while (newinvariant < invariant)
        
      params.log(s"Final descending invariant: $newinvariant\n")
    }
    
    // Save current values for later iterations of the loop
    lastInvariant = invariant
    lastBodyResult = bodyResult
    
    // Annotate results
    ann((this, 1)) = invariant
    if (params.allPPResult) ann((this, 2)) = condition.analyze(invariant)

    // Exit from this loop, hence decrement nesting level
    params.nestingLevel -= 1

    return condition.opposite.analyze(invariant)
  }

  override def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint,U], level: Int, ppspec: PrettyPrinterSpec): String = {
    val spaces = ppspec.indent(level)
    spaces + "while (" + condition.mkString(ppspec.env.names) + ")" + "" +
      (if (ann contains (this, 1)) " " + ppspec.decorator(ann(this, 1)) else "") + " {\n" +
      (if (ann contains (this, 2)) ppspec.indent(level + 1) + ppspec.decorator(ann(this, 2)) + "\n" else "") +
      body.mkString(ann, level + 1, ppspec) + '\n' +
      spaces + '}'
  }
}
