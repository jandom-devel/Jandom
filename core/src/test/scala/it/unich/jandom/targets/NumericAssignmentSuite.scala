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

package it.unich.jandom.targets

import org.mockito.Mockito
import org.mockito.Mockito.verify
import org.mockito.Mockito.when
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.mockito.MockitoSugar

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.NumericalProperty

/**
 * The test suite for NumericAssignment
 * @author Gianluca Amato <gamato@unich.it>
 */
class NumericAssignmentSuite extends AnyFunSuite with MockitoSugar {

  trait MockNumericProperty extends NumericalProperty[MockNumericProperty]
  val TESTVAR = 4

  test("toString method with mocks") {
    val exp = mock[NumericExpression]
    val m = mock[NumericAssignment]
    when(exp.toString) thenReturn "numexp"
    when(m.toString) thenAnswer (Mockito.CALLS_REAL_METHODS)
    when(m.v) thenReturn TESTVAR
    when(m.exp) thenReturn exp
    assertResult(s"v${TESTVAR} := numexp") { m.toString }
  }

  test("analyze Method with mocks") {
    val exp = mock[NumericExpression]
    val m = mock[MockNumericProperty]
    val ass = NumericAssignment(TESTVAR, exp)
    ass.analyze(m)
    verify(exp).assignTo(TESTVAR)(m)
  }

  test("analyze method without mocks") {
    val BoxDouble = BoxDoubleDomain()
    val d = BoxDouble.top(2)
    val la1 = NumericAssignment(0,0)
    val d1 = la1.analyze(d)
    assertResult(BoxDouble(Array(0, Double.NegativeInfinity), Array(0, Double.PositiveInfinity))) { d1 }
    val la2 = NumericAssignment(1, 1)
    val d2 = la2.analyze(d1)
    assertResult(BoxDouble(Array(0, 1), Array(0, 1))) { d2 }
  }
}
