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

package it.unich.sci.jandom
package targets.slil

import domains.{ AbstractProperty, NumericalProperty }
import targets._
import annotations._

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
  import AnalysisPhase._
  import parameters.NarrowingStrategy._
  
  program = this

  /**
   * Returns the environment associated with the program.
   */
  def environment = env

  override def mkString[U <: AbstractProperty](ann: Annotation[U], level: Int, 
      ppspec: PrettyPrinterSpec = new PrettyPrinterSpec(env)) = {
    val spaces = ppspec.indent(level)
    val innerspaces = ppspec.indent(level + 1)
    spaces + "function (" + (inputVars map { v: Int => env(v) }).mkString(",") + ") {\n" +
      (if (ann.get(this, 1) != None) innerspaces + ppspec.decorator(ann(this, 1)) + "\n" else "") +
      stmt.mkString(ann, level + 1, ppspec) + "\n" +
      (if (ann.get(this, 2) != None) innerspaces + ppspec.decorator(ann(this, 2)) + "\n" else "") +
      spaces + "}\n"    
  }

  override def analyzeStmt(params: Parameters)(input: params.Property, 
      phase: AnalysisPhase, ann: Annotation[params.Property]): params.Property = {
    if (params.allPPResult) ann((this, 1)) = input
    val output = params.narrowingStrategy match {
      case Separate => 
        stmt.analyzeStmt(params)(input, Ascending, ann)
     	stmt.analyzeStmt(params)(input, Descending, ann) 
      case _ =>
    	stmt.analyzeStmt(params)(input, Ascending, ann)
    }
    if (params.allPPResult) ann((this, 2)) = output
    return output
  }
}

