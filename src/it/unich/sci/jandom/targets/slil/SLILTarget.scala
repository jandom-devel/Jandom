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
 * (c) 2011 Gianluca Amato
 */

package it.unich.sci.jandom.targets.slil

import it.unich.sci.jandom.domains._
import it.unich.sci.jandom.targets.linearcondition._
import it.unich.sci.jandom.targets.Environment
import it.unich.sci.jandom.targets.LinearForm
import scala.collection.mutable.ListBuffer

/**
 * The target for a simple imperative language similar to the one analyzed
 * by Random.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

case class SLILProgram( val environment: Environment, val inputVars: Iterable[Int], val stmt: SLILStmt ) {     
  private var input: NumericalProperty[_] = null
  private var output: NumericalProperty[_] = null

  override def toString = "function (" +  (inputVars map { v:Int => environment(v - 1) }).mkString(",") + ") {\n"  + 
		(if (input != null) "  " + input + "\n" else "") +
    	stmt.formatString(1,2) + "\n" + 
    	(if (output != null) "  " + output + "\n" else "") + '}'
  
  def analyze[Property <: NumericalProperty[Property]](domain: NumericalDomain[Property]) {    	  
	  val start = domain.full(environment.size)
	  input = start
	  output = stmt.analyze(start)	  	  
  }        
}
  
sealed abstract class SLILStmt { 
  def formatString(indent: Int, indentSize: Int): String
  def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = input
  override def toString = formatString(0,2)
}

case class AssumeStmt(cond: LinearCond) extends SLILStmt {
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = cond.analyze(input)  
  override def formatString(indent: Int, indentSize:Int) = " "*indentSize*indent + "assume(" + cond +")"
}

case class AssignStmt[T](variable: Int, linearForm: LinearForm[T]) (implicit numeric: Numeric[T]) extends SLILStmt {
  import numeric._
  
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = {
    val coefficients = linearForm.coefficients
    input.linearAssignment(variable-1, (coefficients.tail map (x => x.toDouble())).toArray,coefficients.head.toDouble)
  }   
  override def formatString(indent: Int, indentSize: Int) = " "*indentSize*indent + linearForm.env(variable-1) + " = " + linearForm.toString
}

case class CompoundStmt(stmts: Iterable[SLILStmt]) extends SLILStmt {
  val annotations = ListBuffer[NumericalProperty[_]]()
  
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = {
    var current = input
    var first = true
    for (stmt <- stmts) {
      if (first)
        first = false
      else
        annotations += current 
      current = stmt.analyze(current)      
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

case class WhileStmt(condition: LinearCond, body: SLILStmt) extends SLILStmt {  
  var savedInvariant : NumericalProperty[_] = null
 
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property =  {    
    var newinvariant = input
    var invariant = input
    do {      
      invariant = newinvariant
      newinvariant = invariant widening body.analyze(condition.analyze(invariant))
    } while (newinvariant > invariant)          
    do {
      invariant = newinvariant
      newinvariant = invariant narrowing body.analyze(condition.analyze(invariant))      
    } while (newinvariant < invariant)    
    savedInvariant = invariant
    return condition.opposite.analyze(invariant)
  }  

  override def formatString(indent: Int, indentSize: Int) = {
    val spaces = " "*indentSize*indent
    spaces + "while (" + condition +") {\n"  + 
      (if (savedInvariant!=null) spaces + " "*indentSize  + savedInvariant + "\n" else "") + 
      body.formatString(indent+1, indentSize) + '\n' + 
    spaces + '}'
  }
}

case class IfStmt(condition: LinearCond, if_branch: SLILStmt, else_branch: SLILStmt) extends SLILStmt {
  var savedThenAnnotationStart : NumericalProperty[_] = null
  var savedThenAnnotationEnd : NumericalProperty[_] = null
  var savedElseAnnotationStart : NumericalProperty[_] = null
  var savedElseAnnotationEnd : NumericalProperty[_] = null
  
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = {
    val thenAnnotationStart = condition.analyze(input)
    val elseAnnotationStart = condition.opposite.analyze(input)
    val thenAnnotationEnd = if_branch.analyze(thenAnnotationStart)
    val elseAnnotationEnd = else_branch.analyze(elseAnnotationStart)
    savedThenAnnotationStart = thenAnnotationStart
    savedThenAnnotationEnd = thenAnnotationEnd
    savedElseAnnotationStart = elseAnnotationStart
    savedElseAnnotationEnd = elseAnnotationEnd
    return thenAnnotationEnd union elseAnnotationEnd
  }
  
  override def formatString(indent: Int, indentSize: Int) : String = {  
    val spaces = " "*indentSize*indent
    val s = spaces + "if (" + condition.toString + ") {\n" + 
        (if (savedThenAnnotationStart!=null) spaces + " "*indentSize+ savedThenAnnotationStart + "\n" else "") + 
        if_branch.formatString(indent+1,indentSize) + "\n" + 
        (if (savedThenAnnotationEnd!=null) spaces + " "*indentSize+savedThenAnnotationEnd + "\n" else "") + 
      spaces +"} else {\n" + 
        (if (savedElseAnnotationStart!=null) spaces + " "*indentSize + savedElseAnnotationStart + '\n' else "") + 
        else_branch.formatString(indent+1, indentSize) +  "\n" + 
        (if (savedElseAnnotationStart!=null) spaces + " "*indentSize+  savedElseAnnotationEnd + '\n' else "") +
      spaces + '}'
    return s  
  }  
}

case class NopStmt() extends SLILStmt {
  override def formatString(indent: Int, indentSize: Int): String = " "*indentSize*indent + "<no-op>"
}
