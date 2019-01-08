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
import it.unich.jandom.targets.{Annotation, lts}

/**
  * The abstract class for program statements. Each object in SLILStmt represents a statement
  * of a simple imperative language.
  */
abstract class SLILStmt extends SLILTarget {

  import AnalysisPhase._

  /**
    * A method to pretty print a SLILStmt with corresponding annotations.
    *
    * @param ann    the annotation to print together with the program
    * @param ppspec the specification object for pretty printing
    * @param row    the current row
    * @param level  the current indentation level
    * @return the string representation of the program
    */
  def mkString[T <: NumericalProperty[_]](ann: Annotation[ProgramPoint, T], ppspec: SLILPrinterSpec,
                                          row: Int, level: Int): String

  /**
    * The analyzer for a SLIL statement. This methods is different from the one declared in Target since it takes
    * an annotations as a parameter, and update it with the result of the analysis. Moreover, it returns a numerical
    * property as a result instead of an annotation.
    *
    * @param input  the property at the program point before the statement
    * @param params the parameter which control the analysis
    * @param phase  the current analysis phase
    * @param ann    an annotation where to put informations on the inner program points
    * @return the property at the end of the statement
    */
  def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase,
                                      ann: Annotation[ProgramPoint, params.Property]): params.Property

  /**
    * @inheritdoc
    * A statement is analyzed under the assumption that initially variables
    * may assume all possible values.
    */
  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property] = {
    val ann = getAnnotation[params.Property]
    val input = params.domain.top(numvars)
    analyzeStmt(params)(input, AscendingRestart, ann)
    ann
  }

  /**
    * Returns true if `that` is syntactically equal to `this`.
    */
  def syntacticallyEquals(that: SLILStmt): Boolean

  /**
    * Returns the number of variables in the statement. The standard implementation
    * return zero.
    */
  val numvars: Int

  /**
    * This method builds a set of transitions and locations corresponding to the program statement. The initial and
    * final locations are passed as parameters.
    *
    * @param prev the incoming program point
    * @param next the outgoing program point
    * @return a pair `(m,s)` where `m` is map from program points in this statement to locations and `s` is a sequence
    *         of transitions from `prev` to `next`
    */
  def toLTS(prev: lts.Location, next: lts.Location): (Map[ProgramPoint, lts.Location], Seq[lts.Transition])

  val lastPP: Option[ProgramPoint] = None

}
