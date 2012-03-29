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
package it.unich.sci.jandom.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import it.unich.sci.jandom.targets.LinearForm
import it.unich.sci.jandom.targets.Environment
import it.unich.sci.jandom.targets.LinearCondition._
import it.unich.sci.jandom.targets.LTS._

/**
 * Parser for LPInv transition systems
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
object LPInvParser extends JavaTokenParsers {
    private val env = Environment()

    override val whiteSpace = """(\s|#.*\r?\n)+""".r  // handle # as the start of a comment
    
    private def variable: Parser[Int] = 
      ident ^^ { env.getBindingOrAdd(_) }
    
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

    
    // the template part is ignored for now
    val template: Parser[Null] = literal("Template") ~ "(" ~ """[a-zA-Z$,+-]""".r ~ ")" ~ ";" ^^ { case _ => null }         

    val location: Parser[Location] = (literal("location") ~> ident) <~ literal("with") <~ "(" <~ rep(constraint) <~ ")" <~ ";" ^^ { case id => Location(id, Nil) } 
    
    val constraint: Parser[Constraint[Int]] = ident ~ "=" ~ expr ^^ { case v ~ _ ~ lf => Constraint(env.getBindingOrAdd(v),lf) } 	 

    
    val prog = template ~> location
    
   	def parseProgram(s: String) = parseAll(prog,s)
}
