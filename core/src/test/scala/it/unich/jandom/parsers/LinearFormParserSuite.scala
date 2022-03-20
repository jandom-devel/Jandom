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

import org.scalatest.funsuite.AnyFunSuite

import it.unich.jandom.targets.Environment
import it.unich.jandom.domains.numerical.LinearForm

/**
 * Test suite for LinearFormParser.
 * @author Gianluca Amato <gamato@unich.it>
 */
class LinearFormParserSuite extends AnyFunSuite {

  val parser = new TempParser

  class TempParser extends LinearFormParser {
    val env = Environment()
    val variable = ident ^^ { env.getBindingOrAdd(_) }
    def parseExpr(s: String) = parseAll(linform, s)
  }

  test("constant") { 
    assertResult(LinearForm(0)) { parser.parseExpr("0").get }
  }

  test("unary minus") {
    assertResult(LinearForm(-2)) { parser.parseExpr("-2").get }
  }

  test("linear expression") {
    val expParsed = parser.parseExpr("3*x+y-z").get
    val expBuild = LinearForm(0, 3, 1, -1)
    assertResult(expBuild) { expParsed }
  }

}

