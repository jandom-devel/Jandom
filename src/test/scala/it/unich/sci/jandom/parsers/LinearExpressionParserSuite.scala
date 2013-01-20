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

import targets.{ Environment, LinearForm }
import org.scalatest.FunSuite

/**
 * Test suite for LinearExpressionParser
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class LinearExpressionParserSuite extends FunSuite {
  object parser extends LinearExpressionParser {
    val env = Environment()
    val variable = ident ^^ { env.getBindingOrAdd(_) }
    def parseExpr(s: String) = parseAll(expr, s)
  }

  test("linear expression parser") {
    val expParsed = parser.parseExpr("3*x+y-z").get
    val expBuild = LinearForm(Seq(0, 3, 1, -1), Environment("x", "y", "z"))
    expectResult(expBuild) { expParsed }
  }
}
