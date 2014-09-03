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

import it.unich.sci.jandom.domains.numerical.LinearForm
import it.unich.sci.jandom.targets.Environment
import it.unich.sci.jandom.targets.linearcondition.AndCond
import it.unich.sci.jandom.targets.linearcondition.AtomicCond

/**
 * Test suite for LinearExpressionParser
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class LinearConditionParserSuite extends FunSuite {
  object parser extends LinearConditionParser with LinearExpressionParser {
    val env = Environment()
    val variable = ident ^^ { env.getBindingOrAdd(_) }
    def parseExpr(s: String) = parseAll(condition, s)
  }

  test("linear condition parser") {
    val expParsed = parser.parseExpr("3*x+y-z==0 && x<=z").get
    val exp1Build = LinearForm(0, 3, 1, -1)
    val exp2Build = LinearForm(0, 1, 0, -1)
    val expCond = AndCond(AtomicCond(exp1Build, AtomicCond.ComparisonOperators.EQ), AtomicCond(exp2Build, AtomicCond.ComparisonOperators.LTE))
    assertResult(expCond) { expParsed }
  }
}
