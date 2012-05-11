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
import targets.Parameters
import annotations.{ BlackBoard, PerProgramPointAnnotation, EmptyAnnotationType }

/**
 * The abstract class for program statements. Each object in SLILStmt represents a statement
 * of a simple imperative language.
 */
abstract class SLILStmt {
  /**
   * The program this statement is part of.
   */
  private[this] val program: SLILProgram = null

  /**
   * A method to pretty print a SLILStmt with corresponding annotations 
   * @param ann the annotation to print together with the program
   * @param level the current indentation level
   * @param ppspec the specification for the format to use for pretty printing. It defaults to the
   * standard pretty printer specification
   * @return the string representation of the program
   */
  def mkString(ann: PerProgramPointAnnotation[SLILProgram, _], level: Int = 0, ppspec: PrettyPrinterSpec = PrettyPrinterSpec()): String

  /**
   * The analyzer for the SLIL statement.
   * @tparam Property the class of the properties we want to analyze
   * @param input the property at the program point before the statement
   * @param params the parameter which control the analysis
   * @param ann a blackboard where put annotations for the inner program points
   * @return the property at the end of the statement
   */
  def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property, SLILProgram], ann: BlackBoard[SLILProgram]): Property = input

  override def toString = mkString(SLILProgram.SLILProgramPointAnnotationBuilder.apply(program, EmptyAnnotationType))
}
