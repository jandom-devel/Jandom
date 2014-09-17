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

package it.unich.jandom.targets.slil

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.Annotation

/**
 * A class for the compound statement (sequential composition). Each compound statements has
 * several program points, one at the beggining, another at the end, another one between each
 * couple of statements.
 * @param stmts the sequence of statements that form the compound statement
 */
case class CompoundStmt(stmts: SLILStmt*) extends SLILStmt {
  import AnalysisPhase._

  override def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[ProgramPoint, params.Property]): params.Property = {
    var current = input
    var index = 0
    for (stmt <- stmts) {
      if (params.allPPResult) ann((this, index)) = current
      index += 1
      current = stmt.analyzeStmt(params)(current, phase, ann)
    }
    if (params.allPPResult) ann((this, index)) = current
    current
  }

  override def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint, U], ppspec: SLILPrinterSpec, row: Int, level: Int): String = {
    val spaces = ppspec.indent(level)
    val result = new StringBuilder()
    var index = 0
    var decorations = 0
    for (stmt <- stmts) {
      for (p <- ann.get(this, index); deco <- ppspec.decorator(p, row + result.count(_ == '\n'), spaces.size)) {
        result ++= spaces + deco + '\n'
        decorations += 1
      }
      result ++= stmt.mkString(ann, ppspec, row + result.count(_ == '\n'), level)
      index += 1
    }
    for (p <- ann.get(this, index); deco <- ppspec.decorator(p, row + result.count(_ == '\n'), spaces.size)) {
      result ++= spaces + deco + '\n'
    }
    result.toString
  }

  val numvars = (stmts map { _.numvars }).max
}
