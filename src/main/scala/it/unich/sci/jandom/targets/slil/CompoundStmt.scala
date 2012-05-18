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

import domains.NumericalProperty
import targets.Parameters
import annotations.{ BlackBoard, PerProgramPointAnnotation }
import scala.collection.mutable.ListBuffer

/**
 * A class for the compound statement (sequential composition)
 * @param stmts the sequence of statements that form the compound statement
 */
case class CompoundStmt(stmts: Seq[SLILStmt]) extends SLILStmt {
  override def analyze[Property <: NumericalProperty[Property]](input: Property, params: Parameters[Property], ann: Annotation[Property]): Property = {
    var current = input
    var index = 0
    for (stmt <- stmts) {
      if (index > 0 && params.allPPResult) ann((this, index)) = current
      index += 1
      current = stmt.analyze(current, params, ann)
    }
    current
  }

  override def mkString(ann: Annotation[_], level: Int, ppspec: PrettyPrinterSpec): String = {    
    val spaces = ppspec.indent(level)
    val result = new StringBuilder()
    var index = 1
    for (stmt <- stmts) {     
      if (index > 1) result += '\n'        
      result ++= stmt.mkString(ann, level, ppspec)
      if (ann.get(this,index) != None) 
        result ++= '\n' + spaces + ppspec.decorator(ann(this,index))
      index += 1
    }
    result.toString
  }
}
