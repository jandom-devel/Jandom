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

import it.unich.sci.jandom.domains.numerical.BoxDoubleDomain
import it.unich.sci.jandom.targets.Environment

/**
 * Test suite for NumericalProperty
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class NumericalPropertyParserSuite extends FunSuite {
  val env = Environment()
  val parser = new NumericalPropertyParser(env)
  val BoxDouble = BoxDoubleDomain()


  test("numerical property parser") {
    val prop1 = parser.parseProperty("x<=5 && y>=-3", BoxDouble).get
    expectResult( BoxDouble(Array(Double.NegativeInfinity,-3), Array(5,Double.PositiveInfinity)) ) { prop1 }
    parser.closedVariables=true
    val prop2 = parser.parseProperty("x<=5 && y>=-3", BoxDouble).get
    expectResult (prop1) { prop2 }
    expectResult ( false ) { parser.parseProperty("z<=5 && y>=-3", BoxDouble).successful }
  }
}
