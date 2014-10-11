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

package it.unich.jandom.targets.slil

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.Annotation
import it.unich.jandom.targets.NumericCondition
import AnalysisPhase.AnalysisPhase

/**
 * The class for the statement assume. It takes a linear condition as a parameter, and forces this condition to hold. It is
 * somewhat equivalent to "if (not cond) loop-forever".
 * @param cond the linear condition
 */
case class AssumeStmt(cond: NumericCondition) extends SLILStmt {
  import AnalysisPhase._

  override def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[ProgramPoint,params.Property]): params.Property =
    cond.analyze(input)

  override def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint,U],  ppspec: SLILPrinterSpec, row: Int, level:Int) =
    ppspec.indent(level) + "assume(" + cond + ")\n"

  val numvars = cond.dimension
}
