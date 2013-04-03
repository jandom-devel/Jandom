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

import targets.LinearForm
import targets.linearcondition._
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * A trait for parsing integer linear conditions. To be inherited by real parsers. An implementation
 * should define a parser ''expr'' of type ''Parser[LinearForm[Int]]'' and an optional parser
 * ''operator_alias'' of type ''Parser[String]'' for atomic operators additional w.r.t.
 * the standard ones. It provides a parser ''condition'' for the parsing of linear conditions
 * in C/Java notation.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
trait LinearConditionParser extends JavaTokenParsers {
  /**
   * A parser for linear expressions. Should be provided in a real implementation
   */
  protected val linexpr: Parser[LinearForm[Int]]

  /**
   * Parser for comparison operators
   */
  protected def comparison: Parser[AtomicCond.ComparisonOperators.Value] =
    ("==" | "<=" | ">=" | "!=" | "<" | ">" ) ^^ { AtomicCond.ComparisonOperators.withName(_) } |
      failure("invalid comparison operator")

  /**
   * Parser for atomic conditions
   */
  protected def atomic_condition: Parser[LinearCond] =
    "FALSE" ^^ { s => FalseCond } |
    "TRUE" ^^ { s => TrueCond } |
    "brandom" ~ "(" ~ ")" ^^ { s => BRandomCond } |
    linexpr ~ comparison ~ linexpr ^^ { case lf1 ~ op ~ lf2 => AtomicCond(lf1 - lf2, op) }

  /**
   * Parser for linear conditions
   */
  protected def condition: Parser[LinearCond] =
    atomic_condition ~ "&&" ~ condition ^^ { case c1 ~ _ ~ c2 => AndCond(c1, c2) } |
    atomic_condition ~ "||" ~ condition ^^ { case c1 ~ _ ~ c2 => OrCond(c1, c2) } |
    "!" ~> condition ^^ { case c => NotCond(c) } |
    atomic_condition
}
