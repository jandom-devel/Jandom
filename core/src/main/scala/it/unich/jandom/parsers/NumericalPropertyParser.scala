/**
 * Copyright 2013, 2014, 2015 Gianluca Amato <gamato@unich.it>, Francesca Scozzari <fscozzari@unich.it>
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
 * @author Francesca Scozzari <fscozzari@unich.it>
 *
 */
class NumericalPropertyParser(val env: Environment) extends JavaTokenParsers with NumericExpressionParser with NumericConditionParser with VariableParser {
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

