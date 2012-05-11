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

import domains.{ NumericalProperty, NumericalPropertyAnnotation }
import targets.{ Environment, LinearForm, Parameters, Target }
import widenings.Widening
import annotations._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

/**
 * The target for a simple imperative language, similar to the one analyzed
 * by Random. Each program is essentially a function with some input variables and
 * a body, with a single scope which extends to the entire body.
 * @param env the environment for the program
 * @param inputVars the input variables
 * @param stmt the body of the program
 * @author Gianluca Amato <amato@sci.unich.it>
 */
case class SLILProgram(private val env: Environment, private val inputVars: Seq[Int], private val stmt: SLILStmt) extends  SLILStmt with Target {
  type ProgramPoint = (SLILStmt,Int)
  type Tgt = SLILProgram

  def formatString(indent: Int, indentSize: Int, ann: PerProgramPointAnnotation[SLILProgram, _]) =
    "function (" + (inputVars map { v: Int => env(v) }).mkString(",") + ") {\n" +
      (if (ann(this,1) != null) "  " + ann(this,1) + "\n" else "") +
      stmt.formatString(1, 2, ann) + "\n" +
      (if (ann(this,2) != null) "  " + ann(this,2) + "\n" else "") + '}'

  def analyze[Property <: NumericalProperty[Property]](params: Parameters[Property, SLILProgram], bb: BlackBoard[SLILProgram]) {
    val start = params.domain.full(env.size)
    if (params.allPPResult) bb(NumericalPropertyAnnotation)((this,1)) = start
    val output = stmt.analyze(start, params, bb)
    if (params.allPPResult) bb(NumericalPropertyAnnotation)((this,2)) = output
  }
 
  def size = 1
}

/**
 * The companion object for SLILProgram. It defines the AnnotationBuilder for program point annotations.
 */
object SLILProgram {
  /**
   * The annotation builder for program point annotations in SLILProgram's.
   */
  implicit object SLILProgramPointAnnotationBuilder extends PerProgramPointAnnotationBuilder[SLILProgram] {
    def apply[Ann <: AnnotationType](t: SLILProgram, ann: Ann): PerProgramPointAnnotation[SLILProgram, Ann] =
      new PerProgramPointAnnotation[SLILProgram, Ann] {
        private val a = new HashMap[SLILProgram#ProgramPoint, Ann#T]
        def apply(pp: SLILProgram#ProgramPoint) = a.get(pp) match {
          case None => { a(pp) = ann.defaultValue; ann.defaultValue }
          case Some(v) => v
        }
        def update(pp: SLILProgram#ProgramPoint, v: Ann#T) { a(pp) = v }
        def iterator = a.iterator
      }
  }
}
