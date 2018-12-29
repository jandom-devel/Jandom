/**
 * Copyright 2013, 2016, 2018 Gianluca Amato <gianluca.amato@unich.it>
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
import it.unich.jandom.targets.parameters.NarrowingStrategy
import it.unich.jandom.targets.{Annotation, Environment}

/**
 * The target for a simple imperative language, similar to the one analyzed
 * by Random. Each program is essentially a function with some input variables and
 * a body, with a single scope which extends to the entire body.
 * @param env the environment for the program
 * @param inputVars the input variables
 * @param stmt the body of the program
 * @author Gianluca Amato <gamato@unich.it>
 */
case class SLILProgram(env: Environment, inputVars: Seq[Int], stmt: SLILStmt) extends SLILTarget {

  import AnalysisPhase._

  def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint,U],
                                          ppspec: SLILPrinterSpec = SLILPrinterSpecInline(env)): String = {
    val spaces = ppspec.indent(0)
    spaces + "function (" + (inputVars map { v: Int => env(v) }).mkString(",") + ") {\n" +
      stmt.mkString(ann, ppspec, 1, 1)  +
      spaces + "}\n"
  }

  def analyze(params: Parameters): Annotation[ProgramPoint,params.Property] = {
    val input = params.domain.top(env.size)
    val ann = getAnnotation[params.Property]
    params.narrowingStrategy match {
      case NarrowingStrategy.Separate =>
        stmt.analyzeStmt(params)(input, Ascending, ann)
        stmt.analyzeStmt(params)(input, Descending, ann)
      case _ =>
        stmt.analyzeStmt(params)(input, Ascending, ann)
    }
    ann.asInstanceOf[Annotation[ProgramPoint,params.Property]]
  }

  val lastPP: stmt.lastPP.type = stmt.lastPP
}
