/**
 * Copyright 2013 Gianluca Amato
 * 
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
 */

package it.unich.sci.jandom
package parsers

import targets.{ Environment, LinearForm }
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * A trait for parsing integer linear expressions. To be inherited by real parsers. An implementation
 * should define a parser ''variable'' of type ''Parser[Int]'' and provide a variable ''env''
 * of type ''Environment''. The result of variable is the id of the variable in the environment
 * ''env''. It provides a parser ''expr'' for linear expressions.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
trait LinearExpressionParser extends JavaTokenParsers {
  /**
   * Environment for variables
   */
  protected val env: Environment

  /**
   * Parser for variables
   */
  protected val variable: Parser[Int]

  /**
   * Parser for terms
   */
  private val term: Parser[LinearForm[Int]] =
    (opt(wholeNumber <~ "*") ~ variable) ^^ {
      case Some(coeff) ~ v => LinearForm.fromCoefficientVar[Int](coeff.toInt, v + 1, env)
      case None ~ v => LinearForm.fromVar[Int](v + 1, env)
    } |
    wholeNumber ^^ { case coeff => LinearForm.fromCoefficient(coeff.toInt, env) }

  private val term_with_operator: Parser[LinearForm[Int]] =
    "+" ~> term |
    "-" ~> term ^^ { lf => -lf } 
    
  /**
   * Parser for integer linear expressions
   */
  protected val linexpr: Parser[LinearForm[Int]] =
    (term_with_operator | term) ~ rep(term_with_operator) ^^ {
      case lf1 ~ lfarr => (lf1 /: lfarr) { (lfa, lfb) => lfa + lfb }
    } 
}
