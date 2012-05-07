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
import annotations.BlackBoard

/**
 * The abstract class for program statements. Each object in SLILStmt represents a statement
 * of a simple imperative language. 
 */
abstract class SLILStmt {
  /** 
   * The program this statement is part of.
   */
  val program: SLILProgram = null
  
  /**
   * A method to pretty print a SLILStmt. 
   * @param indent the indentation level of the statement
   * @param identSize the size in white spaces of each indentation level
   * @return the string representaton of the program
   */
  def formatString(indent: Int, indentSize: Int): String
  
  /**
   * The analyzer for the SLIL statement.
   * @tparam Property the class of the properties we want to analyze
   * @param input the property at the program point before the statement
   * @param params the parameter which control the analysis
   * @param ann a blackboard where put annotations for the inner program points
   * @return the property at the end of the statement
   */
  def analyze[Property <: NumericalProperty[Property]] (input: Property, params: Parameters[Property,SLILProgram], ann: BlackBoard[SLILProgram]): Property = input
  
  override def toString = formatString(0,2)
}
