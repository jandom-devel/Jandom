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

import it.unich.sci.jandom.domains.NumericalDomain
import it.unich.sci.jandom.domains.NumericalProperty
import it.unich.sci.jandom.targets.Environment
import it.unich.sci.jandom.targets.NarrowingStrategy._
import it.unich.sci.jandom.targets.Target
import it.unich.sci.jandom.targets.slil.AnalysisPhase._
import scala.collection.mutable.HashMap
import it.unich.sci.jandom.targets.Annotation

/**
 * The target for a simple imperative language, similar to the one analyzed
 * by Random. Each program is essentially a function with some input variables and
 * a body, with a single scope which extends to the entire body.
 * @param env the environment for the program
 * @param inputVars the input variables
 * @param stmt the body of the program
 * @author Gianluca Amato <amato@sci.unich.it>
 */
case class SLILProgram(val env: Environment, val inputVars: Seq[Int], val stmt: SingleStmt) extends Target {

  type ProgramPoint = (SLILStmt, Int)
  type Tgt = SLILProgram
  type DomainBase = NumericalDomain

  def getAnnotation[Property] = new HashMap[ProgramPoint,Property] with Annotation[ProgramPoint,Property]

  def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint,U], ppspec: PrettyPrinterSpec = new PrettyPrinterSpec(env)) = {
    val spaces = ppspec.indent(0)
    val innerspaces = ppspec.indent(1)
    spaces + "function (" + (inputVars map { v: Int => env(v) }).mkString(",") + ") {\n" +
      stmt.mkString(ann, 1, ppspec) + "\n" +
      spaces + "}\n"
  }

  override def analyze(params: Parameters): Annotation[ProgramPoint,params.Property] = {
    val stmtParams = params.asInstanceOf[it.unich.sci.jandom.targets.Parameters[SLILStmt]]
    val input = stmtParams.domain.full(env.size)
    val ann = getAnnotation[stmtParams.Property]
    val output = params.narrowingStrategy match {
      case Separate =>
        stmt.analyzeStmt(stmtParams)(input, Ascending, ann)
        stmt.analyzeStmt(stmtParams)(input, Descending, ann)
      case _ =>
        stmt.analyzeStmt(stmtParams)(input, Ascending, ann)
    }
    return ann.asInstanceOf[Annotation[ProgramPoint,params.Property]]
  }
}
