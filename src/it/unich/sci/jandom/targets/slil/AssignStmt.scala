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
import targets.{ LinearForm, Parameters }
import annotations.{ BlackBoard, PerProgramPointAnnotation }

/**
 * The class for the assignment statement "variable := linearForm".
 * @tparam T the type of the numerical expression involved in the assignment. It should be endowed with an implicit Numeric[T] object.
 * @param variable the variable we are assign a value to
 * @param linearForm the linear form of the assignment
 * @param numeric the implicit Numeric[T] object
 */
case class AssignStmt[T](variable: Int, linearForm: LinearForm[T])(implicit numeric: Numeric[T]) extends SLILStmt {
  import numeric._

  override def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property, SLILProgram], ann: BlackBoard[SLILProgram]): Property = {
    val coefficients = linearForm.coefficients
    input.linearAssignment(variable, (coefficients.tail map (x => x.toDouble())).toArray, coefficients.head.toDouble)
  }

  override def formatString(indent: Int, indentSize: Int, ann: PerProgramPointAnnotation[SLILProgram, _]) =
    " " * indentSize * indent + linearForm.env(variable) + " = " + linearForm.toString
}
