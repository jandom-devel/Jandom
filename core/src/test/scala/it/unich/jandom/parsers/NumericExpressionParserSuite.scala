/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
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

import org.scalatest.FunSuite
import it.unich.jandom.targets.Environment
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.targets.NumericExpression._

/**
 * Test suite for NumericExpressionParser
 */
class NumericExpressionParserSuite extends FunSuite {

  val parser = new TempParser

  val x = VariableExpression[Int](0)
  val y = VariableExpression[Int](1)
  val z = VariableExpression[Int](2)

  class TempParser extends NumericExpressionParser {
    val env = Environment()
    val variable = ident ^^ { env.getBindingOrAdd(_) }
    def parseExpr(s: String) = parseAll(numexpr, s)
  }

  test("constant") {
    assertResult(LinearExpression(0)) { parser.parseExpr("0").get }
    assertResult(LinearExpression(-2)) { parser.parseExpr("-2").get }
  }

  test("unary minus") {
    assertResult(- x) { parser.parseExpr("- x").get }
    assertResult(-x+y) { parser.parseExpr("- x + y").get }
  }

  test("linear expression") {
    val expParsed = parser.parseExpr("3*x + y - z").get
    val expBuild = 3 * x + y - z
    assertResult(expBuild) { expParsed }
    assert(expParsed.isInstanceOf[LinearExpression[_]])
  }
    
  test("non-linear expression") {
    val expParsed = parser.parseExpr("(3*x + z*y - z)/x").get
    val expBuild = (3 * x + z*y - z)/x
    assertResult(expBuild) { expParsed }
  }
}

