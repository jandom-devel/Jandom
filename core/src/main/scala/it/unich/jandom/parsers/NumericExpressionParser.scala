/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>, Francesca Scozzari <fscozzari@unich.it>
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
import it.unich.jandom.targets.NumericExpression
import it.unich.jandom.targets.NumericExpression._
import scala.util.parsing.combinator.PackratParsers

/**
 * A trait for parsing numeric expressions. To be inherited by real parsers. An implementation
 * should define a parser ''variable'' of type ''Parser[Int]'' and provide a variable ''env''
 * of type ''Environment''. The result of variable is the id of the variable in the environment
 * ''env''. It provides a parser ''numexpr'' for expressions.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
trait NumericExpressionParser extends JavaTokenParsers with PackratParsers {
  /**
   * Parser for variables.
   */
  protected val variable: Parser[Int]

  /**
   * Parser for parameters.
   */
  protected val parameterVariable: Parser[Int]
  
  /**
   * Parser for multiplication operator. Normally "*", may be overriden in subclasses.
   */
  protected val mulExpr: Parser[Any] = "*"

  /**
   * Parser for division operator. Normally "/", may be overriden in subclasses.
   */
  protected val divExpr: Parser[Any] = "/"

  private val factor: Parser[NumericExpression] =
    "?" ^^ { _ => NonDeterministicExpression } |
      "(" ~> numexpr <~ ")" |
      variable ^^ { v => LinearExpression(LinearForm.v[Double](v)) } |
      "@" ~> parameterVariable ^^ { v => LinearExpression(LinearForm.v[Double](v)) } |
      wholeNumber ^^ { c => LinearExpression(c.toDouble) } |
      "-" ~> factor ^^ { e => - e }

  private val term: PackratParser[NumericExpression] =
    (term <~ mulExpr) ~ factor ^^ { case t ~ f => t * f } |
      (term <~ divExpr) ~ factor ^^ { case t ~ f => t / f } |
      factor

  /**
   * Parser for numeric expressions.
   */
  protected val numexpr: PackratParser[NumericExpression] =
    (numexpr <~ "+") ~ term ^^ { case e ~ t =>  e + t } |
      (numexpr <~ "-") ~ term ^^ { case e ~ t => e - t } |
      term
}

