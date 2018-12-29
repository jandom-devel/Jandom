/**
  * Copyright 2013, 2018 Gianluca Amato
  *
  * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
  * JANDOM is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * JANDOM is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.jandom.targets.slil

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.Annotation

/**
  * The class for the non deterministic assignment "variable := ?".
  *
  * @param variable the variable we are assigning the non-deterministic value to
  */
case class NondetStmt(variable: Int) extends SLILStmt {

  import AnalysisPhase._

  /**
    * At the moment this is highly unprecise, since we forget everything for every variable.
    */
  def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase,
                                               ann: Annotation[ProgramPoint, params.Property]): params.Property = {
    input.nonDeterministicAssignment(variable)
  }

  def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint, U], ppspec: SLILPrinterSpec,
                                                   row: Int, level: Int): String =
    ppspec.indent(level) + "x" + variable + " = ?\n"

  val numvars: Int = variable
}
