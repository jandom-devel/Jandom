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

import it.unich.sci.jandom.targets.slil._
import it.unich.sci.jandom.targets.linearcondition._
import it.unich.sci.jandom.targets.LinearForm
import it.unich.sci.jandom.targets.Environment
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Parser for Random programs. 
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
object RandomParser extends JavaTokenParsers {
    private val env = Environment()
     
    override val whiteSpace = """(\s|#.*\r?\n)+""".r  // handle # as the start of a comment
            
    override def stringLiteral = "\"[^\"]*\"".r   // allow CR in string literals
      
    override def ident = not(literal("function")) ~>  """[a-zA-Z._][\w.]*""".r  // allow . in identifiers
    
    private def variable: Parser[Int] = 
      ident ^^ { env.getBindingOrAdd(_)+1 }
    
	private def term: Parser[LinearForm[Int]] = 
	  (opt(wholeNumber <~ "*") ~ variable) ^^ {
	    case Some(coeff) ~ v => LinearForm.fromCoefficientVar[Int](coeff.toInt,v,env)  
	    case None~v =>  LinearForm.fromVar[Int](v,env)
      } |
      wholeNumber ^^ { case coeff => LinearForm.fromCoefficient(coeff.toInt,env) }	
         
	private def term_with_operator: Parser[LinearForm[Int]] =
	  "+" ~> term |
	  "-" ~> term ^^ { lf => -lf  }	  	
	  
	private def expr: Parser[LinearForm[Int]] = 
	  term ~ rep(term_with_operator) ^^ {
	    case lf1 ~ lfarr => (lf1 /: lfarr) { (lfa, lfb) => lfa + lfb }   
   	  }
	
	private def comparison: Parser[AtomicCond.ComparisonOperators.Value] =
	  ("==" | "<=" | ">=" | "!=" | "<"  | ">") ^^ { s => AtomicCond.ComparisonOperators.withName(s) } |
	  failure("invalid comparison operator")
	
	private def atomic_condition: Parser[LinearCond] =
	  "FALSE" ^^ { s => FalseCond } |
	  "TRUE" ^^ { s => TrueCond } |	 
	  "brandom" ~ "(" ~ ")" ^^ { s => BRandomCond } |
	  expr ~ comparison ~ expr ^^ { case lf1 ~ op ~ lf2 => AtomicCond(lf1-lf2, op)} 	  
	  
	private def condition: Parser[LinearCond] = 
	  atomic_condition |
	  atomic_condition ~ "&&" ~ condition ^^ { case c1 ~ _ ~ c2 => AndCond(c1,c2) } |
	  atomic_condition ~ "||" ~ condition ^^ { case c1 ~ _ ~ c2 => OrCond(c1,c2) } |
	  "!" ~> condition ^^ { case c => NotCond(c) }
	  	
	private def stmt: Parser[SLILStmt] = 
	  "assume" ~> "(" ~> condition <~ ")" ^^ { AssumeStmt(_) } |
	  ( "if" ~> "("  ~> condition <~ ")" ) ~ stmt ~ opt("else" ~> stmt) ^^ { 
	    case c ~ s1 ~ Some(s2) => IfStmt(c,s1,s2)
	    case c ~ s1 ~ None => IfStmt(c,s1,NopStmt())
	  }  |
	  ("while" ~> "(" ~> condition <~")") ~ stmt ^^ {
	    case c ~ s => WhileStmt(c,s)
	  } |
	  "{" ~> repsep(stmt, opt(";")) <~ "}" ^^ { CompoundStmt(_) } | 
	  ident ~ ("=" | "<-") ~ expr ^^ { case v ~ _ ~ lf => AssignStmt(env.getBindingOrAdd(v)+1,lf) } 	 
	  
	private def prog: Parser[SLILProgram] = 
	  (opt(ident) ~> ("=" | "<-") ~> "function" ~>  "(" ~> repsep(variable, ",") <~ ")" ) ~ stmt ^^ {
	    case vars ~ stmt=> SLILProgram(env, vars, stmt) 
	}
	
	private def skip: Parser[String] = """(.|[\r\n])*""".r   // skip until the end of the file
		
	/**
	 * This parser a program, eventually preceded by an if statement used to embed an INTERPROC version
	 * of the program. We are assuming that the first function definition in the file is the function
	 * we want to analyze. Other definitions are input for the trace analyzer in Random which is not
	 * implemented yet.
	 */
	private def progWithCases: Parser[SLILProgram] = 
	  opt("if" ~ "(" ~ "FALSE" ~ ")" ~ stringLiteral) ~> prog <~ skip
	  
	def parseProgram(s: String) = parseAll(progWithCases,s)
}
