/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>, Francesca Scozzari <fscozzari@unich.it>
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

import scala.util.parsing.combinator.RegexParsers
import it.unich.jandom.domains.objects.UP
import it.unich.jandom.targets.Environment

/**
 * A parser for pair sharing properties. It parses strings of the form 
 * "{ ( v1, v2 ), ( v3, v4 ), ... }" where v1, v2, etc.. are variable names.
 * @param varNames the name of variables
 */
class PairSharingParser(val env: Environment) extends RegexParsers with VariableParser {  
  private val jimpleVariable = variable | "@" ~> parameterVariable
  private val pair = ("(" ~> jimpleVariable <~ ",") ~ jimpleVariable <~ ")" ^^ { case x ~ y => new UP(x, y) }
  private val sequence = "{" ~> repsep(pair, ",") <~ "}"
  
  /**
   * Parse the string `s` as a pair sharing property.
   * @param s string to parse
   * @return a list of unordered pairs of numbers 
   */
  def parseProperty(s: String) = parseAll(sequence, s)
}
