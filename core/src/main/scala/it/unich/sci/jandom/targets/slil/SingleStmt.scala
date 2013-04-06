/**
 * Copyright 2013 amato
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

import AnalysisPhase.AnalysisPhase

/**
 * This class represents a statement with two program points, one before and one after the 
 * statement. 
 * @author Gianluca Amato <gamato@unich.it>
 */
case class SingleStmt(stmt: SLILStmt) extends SLILStmt {
  
  override def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[ProgramPoint,params.Property]): params.Property = {
     ann((this,1)) = input
     val result = stmt.analyzeStmt(params)(input, phase, ann)
     ann((this,2)) = result
     result
  }

  override def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint,U], level: Int, ppspec: PrettyPrinterSpec) = {
    val spaces = ppspec.indent(level)
    val result = new StringBuilder()
    if (ann.get(this,1) != None)  
        result ++= spaces ++= ppspec.decorator(ann(this,1)) ++= "\n"
    result ++= stmt.mkString(ann,level,ppspec)
    if (ann.get(this,1) != None)  
        result += '\n' ++= spaces ++= ppspec.decorator(ann(this,2))
    result.toString
  }
}
