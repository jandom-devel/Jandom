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

package it.unich.sci.jandom.targets

import it.unich.sci.jandom.domains._
import scala.collection.mutable.ListBuffer

/**
 * The target and analyzer for a simle imperative language similar to the one analyzed
 * by Random.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

sealed abstract class SLILCond {
  def analyze(input: NumericalDomain): NumericalDomain = input;  
  def opposite : SLILCond;
}
case class AtomicCond[T](lf: LinearForm[T], op: AtomicCond.ComparisonOperators.Value) (implicit numeric: Numeric[T])  extends SLILCond {
  import numeric._;
  
  override def analyze(input: NumericalDomain): NumericalDomain = op match {    
    case AtomicCond.ComparisonOperators.LTE => input.linearInequality( lf.homcoeff, lf.known )
    case AtomicCond.ComparisonOperators.LT => input.linearInequality( lf.homcoeff, lf.known )
    case AtomicCond.ComparisonOperators.GTE => input.linearInequality( (-lf).homcoeff, (-lf).known )
    case AtomicCond.ComparisonOperators.GT => input.linearInequality( (-lf).homcoeff, (-lf).known )
    case AtomicCond.ComparisonOperators.NEQ => input.linearDisequality( lf.homcoeff, lf.known )
    case _ => throw new Exception("Not implemented yet")
  }    
  
  def opposite = new AtomicCond(lf, AtomicCond.ComparisonOperators.opposite(op))
}

case class AndCond(cond1: SLILCond, cond2: SLILCond) extends SLILCond {
  def opposite = new OrCond(cond1.opposite, cond2.opposite)
}

case class OrCond(cond1: SLILCond, cond2: SLILCond) extends SLILCond {
  def opposite = new AndCond(cond1.opposite, cond2.opposite)
}

case class NotCond(cond: SLILCond) extends SLILCond {
  def opposite = cond
}

object AtomicCond {
   object ComparisonOperators extends Enumeration {
      val EQ = Value("==")
      val GT = Value(">")
      val GTE = Value(">=")
      val LT = Value("<")
      val LTE = Value("<=")
      val NEQ = Value("!=") 
      
      def opposite(v: Value):Value = {
        return v match {
          case EQ => NEQ
          case GT => LTE
          case GTE => LT
          case LT => GTE
          case LTE => GT
          case NEQ => EQ
        }
      }
  }   
}

sealed abstract class SLILStmt {  
  def analyze(input: NumericalDomain):NumericalDomain = input
}

case class AssignStmt[T](variable: Int, linearForm: LinearForm[T]) (implicit numeric: Numeric[T]) extends SLILStmt {
  import numeric._
  
  override def analyze(input:NumericalDomain):NumericalDomain = {
	val coefficients = linearForm.coefficients
    input.linearAssignment(variable-1, (coefficients.tail map (x => x.toDouble())).toArray,coefficients.head.toDouble)    
  }
  
  override def toString = "v"+variable + " = " + linearForm.toString
}

case class CompoundStmt(stmts: Iterable[SLILStmt]) extends SLILStmt {
  val annotations = ListBuffer[NumericalDomain]()
  
  override def analyze(input:NumericalDomain):NumericalDomain = {
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
  
  override def toString = {
    val result = new StringBuilder()    
    var remainingAnnotations = annotations
    var first = true
    for (stmt <- stmts) {
      if (first) 
        first = false
      else
        result += '\n'      
      result ++= stmt.toString
      if (! remainingAnnotations.isEmpty) {
        result += '\n'
        result ++= remainingAnnotations.head.toString
        remainingAnnotations =  remainingAnnotations.tail
      }        
    }
    result.toString()
  }  
}

case class WhileStmt(condition: SLILCond, body: SLILStmt) extends SLILStmt {
  var invariant : NumericalDomain = null
  
  override def analyze(input:NumericalDomain):NumericalDomain =   {
    var newinvariant = input
    do {      
      invariant = newinvariant
      newinvariant = invariant widening body.analyze(condition.analyze(invariant)) 
    } while (newinvariant > invariant)          
    do {
      invariant = newinvariant
      newinvariant = invariant narrowing body.analyze(condition.analyze(invariant))      
    } while (newinvariant < invariant)
    return condition.opposite.analyze(invariant)
  }  

  override def toString = {
    "while (" + condition +") { \n"  + invariant + "\n" + body + "\n }"
  }
}

case class IfStmt(condition: SLILCond, if_branch: SLILStmt, else_branch: SLILStmt) extends SLILStmt {
  var thenAnnotationStart : NumericalDomain = null
  var thenAnnotationEnd : NumericalDomain = null
  var elseAnnotationStart : NumericalDomain = null
  var elseAnnotationEnd : NumericalDomain = null
  
  override def analyze(input:NumericalDomain):NumericalDomain = {
    thenAnnotationStart = condition.analyze(input)
    elseAnnotationStart = condition.opposite.analyze(input)
    thenAnnotationEnd = if_branch.analyze(thenAnnotationStart)
    elseAnnotationEnd = if_branch.analyze(elseAnnotationStart)
    return thenAnnotationEnd union elseAnnotationEnd
  }
  
  override def toString : String = {   
    val s = "if " + condition.toString + "{ \n" + thenAnnotationStart + "\n" + if_branch + "\n" + thenAnnotationEnd +  "\n } else { \n" + elseAnnotationStart + "\n" +
             else_branch +  "\n" + elseAnnotationEnd + "}"      
    return s  
  }  
}

case class NopStmt() extends SLILStmt {
  override def toString : String = ""
}

case class SLILProgram(env: Environment, inputVars: Iterable[Int], stmt: SLILStmt) {
  override def toString = "function (" +  env.getVariableNames.mkString(",") + ") {\n"  + input + "\n" + stmt + "\n" + output +"\n" + "}"
  var input:AnyRef = null
  var output:AnyRef = null
  
  def analyze(domain: NumericalDomainFactory) {    
	  val size = env.getNumVariables
	  val start = domain.full(size)
	  input = start
	  output = stmt.analyze(start)	  	  
  }
   
}
