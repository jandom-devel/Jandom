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
package it.unich.sci.jandom.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.Map
import it.unich.sci.jandom.targets._
import sun.org.mozilla.javascript.ast.IfStatement

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

object RandomParser extends JavaTokenParsers {
    var currindex:Int = 1
    val variables = Map[String,Int]()
    
    private def getVariable(v: String):Int  =  variables.get(v) match  {
      case Some(n) => n
      case None => { 
        currindex += 1
        variables.update(v, currindex)
        currindex
      }
    }
       
    def variable: Parser[Int] = 
      ident ^^ { getVariable(_) }
    
	def term: Parser[LinearForm[Int]] = 	
	  (opt(wholeNumber) <~ opt("*")) ~ opt(variable) ^^ { 
	     case Some(coeff)~Some(v) =>  LinearForm.fromCoefficientVar[Int](coeff.toInt,v)  
	     case None~Some(v) =>  LinearForm.fromVar[Int](v)
	     case Some(coeff)~None =>  LinearForm.fromCoefficient(coeff.toInt)
	  }
	
	def term_with_operator: Parser[LinearForm[Int]] =
	  "+" ~> term |
	  "-" ~> term ^^ { lf => -lf  }	  	
	  
	def expr: Parser[LinearForm[Int]] = 
	  term ~ rep(term_with_operator) ^^ {
	    case lf1 ~ lfarr => (lf1 /: lfarr) { (lfa, lfb) => lfa + lfb }   
   	  }
	
	def comparison: Parser[ComparisonOperators.Value] =
	  ("==" | "<=" | ">=" | "!=" | "<"  | ">") ^^ { s => ComparisonOperators.withName(s) }
	
	def condition: Parser[SLILCond] = 
	  expr ~ comparison ~ expr ^^ { case lf1 ~ op ~ lf2 => AtomicCond(lf1-lf2, op)} |
	  condition ~ "&&" ~ condition ^^ { case c1 ~ _ ~ c2 => AndCond(c1,c2) } |
	  condition ~ "||" ~ condition ^^ { case c1 ~ _ ~ c2 => OrCond(c1,c2) } |
	  "!" ~> condition ^^ { case c => NotCond(c) }
	  
	def stmt: Parser[SLILStmt] = 
	  ident ~ ("=" | "<-") ~ expr ^^ { case v ~ _ ~ lf => AssignStmt(getVariable(v),lf) } |
	  ( "if" ~> "("  ~> condition <~ ")" ) ~ stmt ~ opt("else" ~> stmt) ^^ { 
	    case c ~ s1 ~ Some(s2) => IfStmt(c,s1,s2)
	    case c ~ s1 ~ None => IfStmt(c,s1,NopStmt())
	  }  |
	  ("while" ~> "(" ~> condition <~")") ~ stmt ^^ {
	    case c ~ s => WhileStmt(c,s)
	  }
	  "{" ~> repsep(stmt, (";" | "\n")) <~ "}" ^^ { CompoundStmt(_) }
	  
	def program: Parser[SLILProgram] = 
	  (opt(ident) ~> ("=" | "<-") ~> "function" ~> "(" ~> repsep(variable, ",") <~ ")" ) ~ stmt ^^ {
	    case vars ~ s => SLILProgram(variables.keys, vars, s) 
	}
	
	def parseProgram(s: String) = parseAll(program,s)
}
