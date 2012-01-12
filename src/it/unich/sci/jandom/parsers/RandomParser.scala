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

import it.unich.sci.jandom.targets._
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Parser for Random programs. 
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

object RandomParser extends JavaTokenParsers {
    private val env = new Environment();
    
    private def variable: Parser[Int] = 
      ident ^^ { env.getVariableOrAdd(_) }
    
	private def term: Parser[LinearForm[Int]] = 	
	  (opt(wholeNumber) <~ opt("*")) ~ opt(variable) ^^ { 
	     case Some(coeff)~Some(v) =>  LinearForm.fromCoefficientVar[Int](coeff.toInt,v,env)  
	     case None~Some(v) =>  LinearForm.fromVar[Int](v,env)
	     case Some(coeff)~None =>  LinearForm.fromCoefficient(coeff.toInt,env)	    
	  }
	
	private def term_with_operator: Parser[LinearForm[Int]] =
	  "+" ~> term |
	  "-" ~> term ^^ { lf => -lf  }	  	
	  
	private def expr: Parser[LinearForm[Int]] = 
	  term ~ rep(term_with_operator) ^^ {
	    case lf1 ~ lfarr => (lf1 /: lfarr) { (lfa, lfb) => lfa + lfb }   
   	  }
	
	private def comparison: Parser[AtomicCond.ComparisonOperators.Value] =
	  ("==" | "<=" | ">=" | "!=" | "<"  | ">") ^^ { s => AtomicCond.ComparisonOperators.withName(s) }
	
	private def condition: Parser[SLILCond] = 
	  expr ~ comparison ~ expr ^^ { case lf1 ~ op ~ lf2 => AtomicCond(lf1-lf2, op)} |
	  condition ~ "&&" ~ condition ^^ { case c1 ~ _ ~ c2 => AndCond(c1,c2) } |
	  condition ~ "||" ~ condition ^^ { case c1 ~ _ ~ c2 => OrCond(c1,c2) } |
	  "!" ~> condition ^^ { case c => NotCond(c) }
	  
	private def stmt: Parser[SLILStmt] = 
	  ident ~ ("=" | "<-") ~ expr ^^ { case v ~ _ ~ lf => AssignStmt(env.getVariableOrAdd(v),lf) } |
	  ( "if" ~> "("  ~> condition <~ ")" ) ~ stmt ~ opt("else" ~> stmt) ^^ { 
	    case c ~ s1 ~ Some(s2) => IfStmt(c,s1,s2)
	    case c ~ s1 ~ None => IfStmt(c,s1,NopStmt())
	  }  |
	  ("while" ~> "(" ~> condition <~")") ~ stmt ^^ {
	    case c ~ s => WhileStmt(c,s)
	  } |
	  "{" ~> repsep(stmt, (";" | "\n")) <~ "}" ^^ { CompoundStmt(_) }
	  
	private def program: Parser[SLILProgram] = 
	  (opt(ident) ~> ("=" | "<-") ~> "function" ~> "(" ~> repsep(variable, ",") <~ ")" ) ~ stmt ^^ {
	    case vars ~ stmt=> SLILProgram(env, vars, stmt) 
	}
	
	def parseProgram(s: String) = parseAll(program,s)
}
