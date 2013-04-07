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

import it.unich.sci.jandom.domains.{NumericalDomain, NumericalProperty}
import it.unich.sci.jandom.targets.Target
import scala.collection.mutable.HashMap
import it.unich.sci.jandom.targets.Annotation

/**
 * The abstract class for program statements. Each object in SLILStmt represents a statement
 * of a simple imperative language.
 */
abstract class SLILStmt extends SLILTarget {
  import AnalysisPhase._
  
  /**
   * A method to pretty print a SLILStmt with corresponding annotations
   * @param ann the annotation to print together with the program
   * @param level the current indentation level
   * @param ppspec the specification object for pretty printing. It defaults to the
   * standard pretty printer specification
   * @return the string representation of the program
   */
  def mkString[T <: NumericalProperty[_]](ann: Annotation[ProgramPoint,T], level: Int = 0, ppspec: PrettyPrinterSpec): String
  
  /**
   * The analyzer for a SLIL statement. This methods is different from the one declared in Target since it takes
   * an annotations as a parameter, and update it with the result of the analysis. Moreover, it returns a numerical
   * property as a result instead of an annotation.
   * @tparam Property the class of the properties we want to analyze
   * @param input the property at the program point before the statement
   * @param params the parameter which control the analysis
   * @param phase the current analysis phase
   * @param ann an annotation where to put informations on the inner program points
   * @return the property at the end of the statement
   */
  def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, 
      ann: Annotation[ProgramPoint,params.Property]): params.Property = input

  /**
   * @inheritdoc
   * A statement is analyzed under the assumption that initially variables
   * may assume all possible values.
   */
  def analyze(params: Parameters): Annotation[ProgramPoint,params.Property] = {
    val ann = getAnnotation[params.Property]
    val input = params.domain.full(0)
    analyzeStmt(params)(input, AscendingRestart, ann)
    return ann
  }
          
  /**
   * Returns the number of variables in the statement. The standard implementation 
   * return zero.
   */
  val numvars: Int
}
