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
import annotations.BlackBoard
import scala.collection.mutable.ListBuffer

/**
 * A class for the compound statement (sequential composition)
 * @param stmts the sequence of statements that form the compound statement
 */
case class CompoundStmt(stmts: Seq[SLILStmt]) extends SLILStmt {
  val annotations = ListBuffer[NumericalProperty[_]]()
  
  override def analyze[Property <: NumericalProperty[Property]] (input: Property, params: Parameters[Property,SLILProgram], ann: BlackBoard[SLILProgram]): Property = {
    var current = input
    var first = true
    for (stmt <- stmts) {
      if (first)
        first = false
      else
        annotations += current 
      current = stmt.analyze(current, params, ann)      
    }  
    current
  }
  
  override def formatString(indent: Int, indentSize: Int) = {
    val spaces = " "*indentSize*indent
    val result = new StringBuilder()    
    var remainingAnnotations = annotations
    var first = true
    for (stmt <- stmts) {
      if (first) 
        first = false
      else
        result += '\n'      
      result ++= stmt.formatString(indent,indentSize)
      if (! remainingAnnotations.isEmpty) {
        result += '\n'
        result ++= spaces  + remainingAnnotations.head.toString
        remainingAnnotations =  remainingAnnotations.tail
      }        
    }
    result.toString()
  }  
}
