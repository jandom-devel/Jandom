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
import targets.Parameters
import targets.linearcondition.LinearCond
import annotations.{ BlackBoard, PerProgramPointAnnotation }

/**
 * The class for an if/then/else statement
 * @param condition the guard of the statement
 * @param then_branch the statement to execute when the guard is true
 * @param else the statement to execute when the guard is false
 */

case class IfStmt(condition: LinearCond, then_branch: SLILStmt, else_branch: SLILStmt) extends SLILStmt {

  override def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property], ann: Annotation): Property = {
    val thenStart = condition.analyze(input)
    val elseStart = condition.opposite.analyze(input)
    val thenEnd = then_branch.analyze(thenStart, params, ann)
    val elseEnd = else_branch.analyze(elseStart, params, ann)
    if (params.allPPResult) {
      ann((this, 1)) = thenStart
      ann((this, 2)) = elseStart
      ann((this, 3)) = thenEnd
      ann((this, 4)) = elseEnd
    }
    return thenEnd union elseEnd
  }

  override def mkString(ann: PerProgramPointAnnotation[SLILStmt, _], level: Int, ppspec: PrettyPrinterSpec): String = {
    val spaces = ppspec.indent(level)
    val innerspaces = ppspec.indent(level+1)
    val s = spaces + "if (" + condition.toString + ") {\n" +
      (if (ann(this, 1) != null) innerspaces + ppspec.decorator(ann(this, 1)) + "\n" else "") +
      then_branch.mkString(ann,level+1,ppspec) + "\n" +
      (if (ann(this, 3) != null) innerspaces + ppspec.decorator(ann(this, 3)) + "\n" else "") +
      spaces + "} else {\n" +
      (if (ann(this, 2) != null) innerspaces + ppspec.decorator(ann(this, 2)) + '\n' else "") +
      else_branch.mkString(ann,level+1,ppspec) + "\n" +
      (if (ann(this, 4) != null) innerspaces + ppspec.decorator(ann(this, 4)) + '\n' else "") +
      spaces + '}'
    return s
  }
}
