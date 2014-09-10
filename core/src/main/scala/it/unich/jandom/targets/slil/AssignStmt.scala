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
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.targets.Annotation

import AnalysisPhase.AnalysisPhase

/**
 * The class for the assignment statement "variable := linearForm".
 * @tparam T the type of the numerical expression involved in the assignment. It should be endowed with an implicit Numeric[T] object.
 * @param variable the variable we are assign a value to
 * @param linearForm the linear form of the assignment
 * @param numeric the implicit Numeric[T] object
 */
case class AssignStmt[T](variable: Int, linearForm: LinearForm[T])(implicit numeric: Numeric[T]) extends SLILStmt {
  import numeric._
  import AnalysisPhase._

  override def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[ProgramPoint,params.Property]): params.Property =
    input.linearAssignment(variable, linearForm.toDouble)

  override def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint,U], level: Int, ppspec: PrettyPrinterSpec) =
    ppspec.indent(level) + ppspec.env(variable) + " = " + linearForm.mkString(ppspec.env.names) + '\n'

  val numvars = linearForm.dimension
}
