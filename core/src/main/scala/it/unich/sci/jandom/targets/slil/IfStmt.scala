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
package targets.slil

import domains.NumericalProperty
import targets.Parameters
import targets.linearcondition.LinearCond
import annotations.{ BlackBoard, PerProgramPointAnnotation }
import it.unich.sci.jandom.domains.AbstractProperty

/**
 * The class for an if/then/else statement
 * @param condition the guard of the statement
 * @param then_branch the statement to execute when the guard is true
 * @param else the statement to execute when the guard is false
 */

case class IfStmt(condition: LinearCond, then_branch: SLILStmt, else_branch: SLILStmt) extends SLILStmt {
  import AnalysisPhase._

  override def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[params.Property]): params.Property = {
    
    val thenStart = condition.analyze(input)
    val elseStart = condition.opposite.analyze(input)
    val thenEnd = then_branch.analyzeStmt(params)(thenStart, phase, ann)
    val elseEnd = else_branch.analyzeStmt(params)(elseStart, phase, ann)
    if (params.allPPResult) {
      ann((this, 1)) = thenStart
      ann((this, 2)) = elseStart
      ann((this, 3)) = thenEnd
      ann((this, 4)) = elseEnd
    }
    return thenEnd union elseEnd
  }

  override def mkString[U <: AbstractProperty](ann: Annotation[U], level: Int, ppspec: PrettyPrinterSpec): String = {
    val spaces = ppspec.indent(level)
    val innerspaces = ppspec.indent(level+1)
    val s = spaces + "if (" + condition.toString + ") {\n" +
      (if (ann.get(this, 1) != None) innerspaces + ppspec.decorator(ann(this, 1)) + "\n" else "") +
      then_branch.mkString(ann,level+1,ppspec) + "\n" +
      (if (ann.get(this, 3) != None) innerspaces + ppspec.decorator(ann(this, 3)) + "\n" else "") +
      spaces + "} else {\n" +
      (if (ann.get(this, 2) != None) innerspaces + ppspec.decorator(ann(this, 2)) + '\n' else "") +
      else_branch.mkString(ann,level+1,ppspec) + "\n" +
      (if (ann.get(this, 4) != None) innerspaces + ppspec.decorator(ann(this, 4)) + '\n' else "") +
      spaces + '}'
    return s
  }
}
