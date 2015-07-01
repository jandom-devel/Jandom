/**
 * Copyright 2013, 2014 Gianluca Amato <gamato@unich.it>, Francesca Scozzari <fscozzari@unich.it>
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
import it.unich.jandom.targets.NumericCondition
import it.unich.jandom.targets.NumericCondition._

/**
 * A trait for parsing integer linear conditions. To be inherited by real parsers. An implementation
 * should define a parser ''expr'' of type ''Parser[LinearForm[Int]]'' and an optional parser
 * ''operator_alias'' of type ''Parser[String]'' for atomic operators additional w.r.t.
 * the standard ones. It provides a parser ''condition'' for the parsing of linear conditions
 * in C/Java notation.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
trait NumericConditionParser extends JavaTokenParsers {
  /**
   * A parser for linear forms. Should be provided in a real implementation.
   */
  protected val numexpr: Parser[NumericExpression]

  /**
   * Parser for comparison operators.
   */
  protected val comparison: Parser[ComparisonOperators.Value] =
    ("==" | "<=" | ">=" | "!=" | "<" | ">") ^^ { ComparisonOperators.withName(_) } |
      "=" ^^ { s => ComparisonOperators.withName("==") } |
      "<>" ^^ { s => ComparisonOperators.withName("!=") } |
      failure("invalid comparison operator")

  /**
   * Parser for atomic conditions.
   */
  private val atomic_condition: Parser[NumericCondition] =
    ("FALSE" | "false") ^^ { s => FalseCond } |
      ("TRUE" | "true") ^^ { s => TrueCond } |
      "brandom" ~ "(" ~ ")" ^^ { s => BRandomCond } |
      numexpr ~ comparison ~ numexpr ^^ { case e1 ~ op ~ e2 => AtomicCond(e1, op, e2) }

  private val basic_condition: Parser[NumericCondition] =
    "!" ~> numcondition ^^ { case c => NotCond(c) } |
      "(" ~> numcondition <~ ")" |
      atomic_condition

  private val conjunction_condition: Parser[NumericCondition] =
    basic_condition ~ "&&" ~ conjunction_condition ^^ { case c1 ~ _ ~ c2 => AndCond(c1, c2) } |
      basic_condition

  /**
   * Parser for linear conditions.
   */
  protected val numcondition: Parser[NumericCondition] =
    conjunction_condition ~ "||" ~ numcondition ^^ { case c1 ~ _ ~ c2 => OrCond(c1, c2) } |
      conjunction_condition
}

