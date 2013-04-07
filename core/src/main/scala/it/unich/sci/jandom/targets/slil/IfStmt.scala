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
import it.unich.sci.jandom.targets.Annotation
import it.unich.sci.jandom.targets.linearcondition.LinearCond

/**
 * The class for an if/then/else statement
 * @param condition the guard of the statement
 * @param then_branch the statement to execute when the guard is true
 * @param else the statement to execute when the guard is false
 */

case class IfStmt(condition: LinearCond, then_branch: SLILStmt, else_branch: SLILStmt) extends SLILStmt {
  import AnalysisPhase._

  override def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[ProgramPoint,params.Property]): params.Property = {
    
    val thenStart = condition.analyze(input)
    val elseStart = condition.opposite.analyze(input)
    val thenEnd = then_branch.analyzeStmt(params)(thenStart, phase, ann)
    val elseEnd = else_branch.analyzeStmt(params)(elseStart, phase, ann)
    return thenEnd union elseEnd
  }

  override def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint,U], level: Int, ppspec: PrettyPrinterSpec): String = {
    val spaces = ppspec.indent(level)
    val innerspaces = ppspec.indent(level+1)
    val s = spaces + "if (" + condition.mkString(ppspec.env.names) + ") {\n" +
      then_branch.mkString(ann,level+1,ppspec) +
      spaces + "} else {\n" +
      else_branch.mkString(ann,level+1,ppspec) +
      spaces + "}\n"
    return s
  }
}
