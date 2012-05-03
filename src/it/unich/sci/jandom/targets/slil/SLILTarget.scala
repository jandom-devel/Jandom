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

package it.unich.sci.jandom
package targets.slil

import domains._
import targets.linearcondition._
import targets.{Environment, LinearForm, Parameters, Target}
import annotations._
import widenings.Widening
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

/**
 * The target for a simple imperative language similar to the one analyzed
 * by Random.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class SLILProgram( val environment: Environment, val inputVars: Iterable[Int], val stmt: SLILStmt ) extends Target  {        
  
  private var input: NumericalProperty[_] = null
  private var output: NumericalProperty[_] = null
  
  type ProgramPoint = Int
  type Tgt = SLILProgram

  override def toString = "function (" +  (inputVars map { v:Int => environment(v) }).mkString(",") + ") {\n"  + 
		(if (input != null) "  " + input + "\n" else "") +
    	stmt.formatString(1,2) + "\n" + 
    	(if (output != null) "  " + output + "\n" else "") + '}'
      	
  def size = 1
  
  def analyze[Property <: NumericalProperty[Property]](params: Parameters[Property,SLILProgram], ann: BlackBoard[SLILProgram]) {    	  
	  val start = params.domain.full(environment.size)
	  input = start
	  output = stmt.analyze(start, params, ann)	  	  
  }        
}
  
sealed abstract class SLILStmt {
  val program: SLILProgram = null
  
  def formatString(indent: Int, indentSize: Int): String
  def analyze[Property <: NumericalProperty[Property]] (input: Property, params: Parameters[Property,SLILProgram], ann: BlackBoard[SLILProgram]): Property = input
  override def toString = formatString(0,2)
}

case class AssumeStmt(cond: LinearCond) extends SLILStmt {
  override def analyze[Property <: NumericalProperty[Property]] (input: Property, params: Parameters[Property,SLILProgram], ann: BlackBoard[SLILProgram]): Property = cond.analyze(input)  
  override def formatString(indent: Int, indentSize:Int) = " "*indentSize*indent + "assume(" + cond +")"
}

case class AssignStmt[T](variable: Int, linearForm: LinearForm[T]) (implicit numeric: Numeric[T]) extends SLILStmt {
  import numeric._
  
  override def analyze[Property <: NumericalProperty[Property]] (input: Property, params: Parameters[Property,SLILProgram], ann: BlackBoard[SLILProgram]): Property = {
    val coefficients = linearForm.coefficients
    input.linearAssignment(variable, (coefficients.tail map (x => x.toDouble())).toArray,coefficients.head.toDouble)
  }   
  override def formatString(indent: Int, indentSize: Int) = " "*indentSize*indent + linearForm.env(variable) + " = " + linearForm.toString
}

case class CompoundStmt(stmts: Iterable[SLILStmt]) extends SLILStmt {
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

case class WhileStmt(condition: LinearCond, body: SLILStmt) extends SLILStmt {  
  var savedInvariant : NumericalProperty[_] = null
  var savedFirst: NumericalProperty[_] = null
  private var widening: Widening = null
 
  override def analyze[Property <: NumericalProperty[Property]] (input: Property, params: Parameters[Property,SLILProgram], ann: BlackBoard[SLILProgram]): Property =  {    
    var newinvariant = input
    var invariant = input
    if (widening==null) widening = params.wideningFactory.widening
    do {      
      invariant = newinvariant
      newinvariant = widening(invariant, input union body.analyze(condition.analyze(invariant), params, ann))
    } while (newinvariant > invariant)          
    do {
      invariant = newinvariant
      newinvariant = params.narrowing[SLILProgram](invariant, input union body.analyze(condition.analyze(invariant),params, ann), ann, this.hashCode)      
    } while (newinvariant < invariant)    
    savedInvariant = invariant
    savedFirst = condition.analyze(invariant)
    return condition.opposite.analyze(invariant)
  }  

  override def formatString(indent: Int, indentSize: Int) = {
    val spaces = " "*indentSize*indent
    spaces + "while (" + condition +")"  + 
      (if (savedInvariant!=null) " "+savedInvariant else "") + " {\n" +
      spaces + " "*indentSize + savedFirst + "\n" +
      body.formatString(indent+1, indentSize) + '\n' + 
    spaces + '}'
  }
}

case class IfStmt(condition: LinearCond, if_branch: SLILStmt, else_branch: SLILStmt) extends SLILStmt {
  var savedThenAnnotationStart : NumericalProperty[_] = null
  var savedThenAnnotationEnd : NumericalProperty[_] = null
  var savedElseAnnotationStart : NumericalProperty[_] = null
  var savedElseAnnotationEnd : NumericalProperty[_] = null
  
  override def analyze[Property <: NumericalProperty[Property]] (input: Property, params: Parameters[Property,SLILProgram], ann: BlackBoard[SLILProgram]): Property = {
    val thenAnnotationStart = condition.analyze(input)
    val elseAnnotationStart = condition.opposite.analyze(input)
    val thenAnnotationEnd = if_branch.analyze(thenAnnotationStart,params, ann)
    val elseAnnotationEnd = else_branch.analyze(elseAnnotationStart,params, ann)
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

object SLILProgram {
 
   implicit object SLILProgramPointAnnotationBuilder extends PerProgramPointAnnotationBuilder[SLILProgram] {
	 def apply[Ann <: AnnotationType] (t: SLILProgram, ann: Ann): PerProgramPointAnnotation[SLILProgram,Ann] = 
	   new PerProgramPointAnnotation[SLILProgram,Ann]{
	     private val a= new HashMap[SLILProgram#ProgramPoint,Ann#T]
	     def apply(pp: SLILProgram#ProgramPoint) = a.get(pp) match {
	       case None => { a(pp) = ann.defaultValue; ann.defaultValue }
	       case Some(v) => v
	     }
	     def update(pp: SLILProgram#ProgramPoint, v: Ann#T) { a(pp) = v }
	     def iterator = a.iterator
	 }
  }  
}
