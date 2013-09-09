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

package it.unich.sci.jandom.parsers

import org.scalatest.FunSuite

import it.unich.sci.jandom.targets.Environment
import it.unich.sci.jandom.domains.numerical.LinearForm

/**
 * Test suite for LinearExpressionParser
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class LinearExpressionParserSuite extends FunSuite {
  class TempParser extends LinearExpressionParser {
    val env = Environment()
    val variable = ident ^^ { env.getBindingOrAdd(_) }
    def parseExpr(s: String) = parseAll(linexpr, s)
  }

  test("linear expression parser") {
    val parser = new TempParser
    val expParsed = parser.parseExpr("3*x+y-z").get
    val expBuild = LinearForm(0, 3, 1, -1)
    expectResult(expBuild) { expParsed }
  }

  test("unary minus") {
    val parser = new TempParser
    val expParsed = parser.parseExpr("- 2").get
    expectResult(LinearForm(-2)) { expParsed }
  }
}
