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
 * The class for a while statement.
 * @param condition the guard of the statement
 * @param body the body of the statement
 */
case class WhileStmt(condition: LinearCond, body: SLILStmt) extends SLILStmt {
  var savedInvariant: NumericalProperty[_] = null
  var savedFirst: NumericalProperty[_] = null

  override def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property, SLILProgram], bb: BlackBoard[SLILProgram]): Property = {
    var newinvariant = input
    var invariant = input
    val widening = params.wideningFactory(this, 1)
    val narrowing = params.narrowingFactory(this, 1)
    do {
      invariant = newinvariant
      newinvariant = widening(invariant, input union body.analyze(condition.analyze(invariant), params, bb))
    } while (newinvariant > invariant)
    do {
      invariant = newinvariant
      newinvariant = narrowing(invariant, input union body.analyze(condition.analyze(invariant), params, bb))
    } while (newinvariant < invariant)
    val ann = bb(NumericalPropertyAnnotation)
    ann((this, 1)) = invariant
    if (params.allPPResult) ann((this, 2)) = condition.analyze(invariant)
    return condition.opposite.analyze(invariant)
  }

  override def formatString(indent: Int, indentSize: Int, ann: PerProgramPointAnnotation[SLILProgram, _]) = {
    val spaces = " " * indentSize * indent
    spaces + "while (" + condition + ")" +
      (if (ann(this, 1) != null) " " + ann(this, 1) else "") + " {\n" +
      (if (ann(this, 2) != null) spaces + " " * indentSize + ann(this, 2) + "\n" else "") +
      body.formatString(indent + 1, indentSize, ann) + '\n' +
      spaces + '}'
  }
}
