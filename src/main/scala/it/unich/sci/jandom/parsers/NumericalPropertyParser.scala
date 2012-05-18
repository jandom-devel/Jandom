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

package it.unich.sci.jandom
package parsers

import targets.Environment
import domains.{ NumericalDomain, NumericalProperty }
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * A parser for numerical properties.
 * @param env environment to use for parsing the property
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class NumericalPropertyParser(val env: Environment) extends JavaTokenParsers with LinearExpressionParser with LinearConditionParser {
  /**
   * If this variable is false, unrecognized variables will be treated as errors, otherwise they will be
   * added to the environment
   */
  var closedVariables = false

  /**
   * Parser for variables
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
   * Parser for properties
   */
  protected def property[Property <: NumericalProperty[Property]](domain: NumericalDomain[Property]) =
    condition ^^ { _.analyze(domain.full(env.size)) }
  
  /**
   * Parsing function.
   * @tparam Property the type of the numerical property we want
   * @param s string to parse
   * @param domain the numerical domain corresponding to the type Property
   * @return a ParseResult[Property] with the result with the parsed property, or corresponding error condition 
   */
  def parseProperty[Property <: NumericalProperty[Property]](s: String, domain: NumericalDomain[Property]) =
    parseAll(property(domain),s)
}
