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
import targets.{ Environment, LinearForm, Target }
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
case class SLILProgram(private val env: Environment, private val inputVars: Seq[Int], private val stmt: SLILStmt) extends SLILStmt {

  program = this

  /**
   * Returns the environment associated with the program.
   */
  def environment = env

  override def mkString(ann: SLILStmt#Annotation[_], level: Int, ppspec: PrettyPrinterSpec) = {
    val spaces = ppspec.indent(level)
    val innerspaces = ppspec.indent(level + 1)
    spaces + "function (" + (inputVars map { v: Int => env(v) }).mkString(",") + ") {\n" +
      (if (ann.get(this, 1) != None) innerspaces + ppspec.decorator(ann(this, 1)) + "\n" else "") +
      stmt.mkString(ann, level + 1, ppspec) + "\n" +
      (if (ann.get(this, 2) != None) innerspaces + ppspec.decorator(ann(this, 2)) + "\n" else "") +
      spaces + '}'    
  }

  override def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property], ann: SLILStmt#Annotation[Property]): Property = {
    if (params.allPPResult) ann((this, 1)) = input
    val output = stmt.analyze(input, params, ann)
    if (params.allPPResult) ann((this, 2)) = output
    return output
  }
}

