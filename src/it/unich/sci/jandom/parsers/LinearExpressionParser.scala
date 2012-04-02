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
import it.unich.sci.jandom.targets._


/**
 * A trait for parsing linear expressions. To be inherited by real parsers. An implementation
 * should define a parser ''variable'' of type ''Parser[Int]'' and provide a variable ''env''
 * of type ''Environment''. 
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
abstract trait LinearExpressionParser extends JavaTokenParsers {    
    protected val env: Environment
	protected val variable: Parser[Int]
    
	protected val term: Parser[LinearForm[Int]] = 
	  (opt(wholeNumber <~ "*") ~ variable) ^^ {
	    case Some(coeff) ~ v => LinearForm.fromCoefficientVar[Int](coeff.toInt,v,env)  
	    case None~v =>  LinearForm.fromVar[Int](v,env)
      } |
      wholeNumber ^^ { case coeff => LinearForm.fromCoefficient(coeff.toInt,env) }	
         
	private val term_with_operator: Parser[LinearForm[Int]] =
	  "+" ~> term |
	  "-" ~> term ^^ { lf => -lf  }	  	
	  
	protected val expr: Parser[LinearForm[Int]] = 
	  term ~ rep(term_with_operator) ^^ {
	    case lf1 ~ lfarr => (lf1 /: lfarr) { (lfa, lfb) => lfa + lfb }   
   	  }	
}
