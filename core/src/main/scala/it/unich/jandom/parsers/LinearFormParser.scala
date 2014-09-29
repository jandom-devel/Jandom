/**
 * Copyright 2013, 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.parsers

import scala.util.parsing.combinator.JavaTokenParsers

import it.unich.jandom.domains.numerical.LinearForm

/**
 * A trait for parsing linear forms. To be inherited by real parsers. An implementation
 * should define a parser ''variable'' of type ''Parser[Int]'' and provide a variable ''env''
 * of type ''Environment''. The result of the `variable` parser should be the id of the 
 * variable in the environmen ''env''. It provides a parser ''linform'' for linear forms.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait LinearFormParser extends JavaTokenParsers {
  /**
   * Parser for variables.
   */
  protected val variable: Parser[Int]

  /**
   * Parser for multiplication operator.
   */    
  protected val mulExpr: Parser[Any] = "*"
  
  /**
   * Parser for terms.
   */
  private val term: Parser[LinearForm[Int]] =
    (opt(wholeNumber <~ mulExpr) ~ variable) ^^ {
      case Some(coeff) ~ v => LinearForm(0, v -> coeff.toInt)
      case None ~ v => LinearForm.v[Int](v)
    } |
    wholeNumber ^^ { case coeff => coeff.toInt }

  private val term_with_operator: Parser[LinearForm[Int]] =
    "+" ~> term |
    "-" ~> term ^^ { lf => -lf }

  /**
   * Parser for integer linear expressions.
   */
  protected val linform: Parser[LinearForm[Int]] =
    (term_with_operator | term) ~ rep(term_with_operator) ^^ {
      case lf1 ~ lfarr => (lf1 /: lfarr) { (lfa, lfb) => lfa + lfb }
    }
}
