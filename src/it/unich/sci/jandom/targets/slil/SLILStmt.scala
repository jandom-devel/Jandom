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

import targets.{ Target }
import domains.{ NumericalProperty, NumericalPropertyAnnotation }
import annotations._
import scala.collection.mutable.HashMap

/**
 * The abstract class for program statements. Each object in SLILStmt represents a statement
 * of a simple imperative language.
 */
abstract class SLILStmt extends Target {
  type ProgramPoint = (SLILStmt, Int)
  type Tgt = SLILStmt

  /**
   * Program associated with this statement.
   */
  protected var program: SLILProgram = null

  /**
   * A method to pretty print a SLILStmt with corresponding annotations
   * @param ann the annotation to print together with the program
   * @param level the current indentation level
   * @param ppspec the specification object for pretty printing. It defaults to the
   * standard pretty printer specification
   * @return the string representation of the program
   */
  def mkString(ann: PerProgramPointAnnotation[SLILStmt, _], level: Int = 0, ppspec: PrettyPrinterSpec = PrettyPrinterSpec()): String

  /**
   * The analyzer for a SLIL statement. This methods is different from the one declared in Target since it takes
   * an annotations as a parameter, and update it with the result of the analysis. Moreover, it returns a numerical
   * property as a result instead of an annotation.
   * @tparam Property the class of the properties we want to analyze
   * @param input the property at the program point before the statement
   * @param params the parameter which control the analysis
   * @param ann an annotation where to put informations on the inner program points
   * @return the property at the end of the statement
   */
  def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property], ann: Annotation): Property = input

  def analyze[Property <: NumericalProperty[Property]](params: Parameters[Property]): Annotation = {
    val ann = SLILStmt.SLILProgramPointAnnotationBuilder(this, NumericalPropertyAnnotation)
    val input = params.domain.full(program.environment.size)
    analyze(input, params, ann)
    return ann
  }
    
  def size = 1

  override def toString = mkString(SLILStmt.SLILProgramPointAnnotationBuilder(this, EmptyAnnotationType))
}

/**
 * The companion object for SLILStmt. It defines the AnnotationBuilder for program point annotations.
 */
object SLILStmt {
  /**
   * The annotation builder for program point annotations in statements.
   */
  implicit object SLILProgramPointAnnotationBuilder extends PerProgramPointAnnotationBuilder[SLILStmt] {
    def apply[Ann <: AnnotationType](t: SLILStmt, ann: Ann): PerProgramPointAnnotation[SLILStmt, Ann] =
      new PerProgramPointAnnotation[SLILStmt, Ann] {
        private val a = new HashMap[SLILStmt#ProgramPoint, Ann#T]
        def apply(pp: SLILStmt#ProgramPoint) = a.get(pp) match {
          case None => { a(pp) = ann.defaultValue; ann.defaultValue }
          case Some(v) => v
        }
        def update(pp: SLILStmt#ProgramPoint, v: Ann#T) { a(pp) = v }
        def iterator = a.iterator
      }
  }
}
