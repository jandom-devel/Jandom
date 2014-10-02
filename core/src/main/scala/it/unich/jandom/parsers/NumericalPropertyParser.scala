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

import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.targets.Environment

/**
 * A parser for numerical properties.
 * @param env environment to use for parsing the property.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class NumericalPropertyParser(val env: Environment) extends JavaTokenParsers with NumericExpressionParser with NumericConditionParser {
  /**
   * If this variable is false, unrecognized variables will be treated as errors, otherwise they will be
   * added to the environment.
   */
  var closedVariables = false

  /**
   * Parser for variables.
   */
  override val variable: Parser[Int] = new Parser[Int] {
    def apply(in: Input) = ident(in) match {
      case Success(i, in1) => env.getBinding(i) match {
        case Some(v) => Success(v, in1)
        case None => if (closedVariables) Failure("Unexpected variable", in1) else Success(env.addBinding(i), in1)
      }
      case default => default.asInstanceOf[ParseResult[Int]]
    }
  }

  /**
   * Parser for properties.
   */
  protected def property(domain: NumericalDomain) = numcondition ^^ { _.analyze(domain.top(env.size)) }

  /**
   * Parsing function.
   * @param s string to parse
   * @param domain the numerical domain corresponding to the type Property
   * @return a ParseResult[dom.Property] with the parsed property, or corresponding error condition
   */
  def parseProperty(s: String, domain: NumericalDomain) = parseAll(property(domain),s)
}

