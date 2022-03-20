/**
 * Copyright 2014, 2016 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.targets

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito
import org.mockito.Mockito.when
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.targets.NumericExpression._

/**
 * The test suite for NumericExpression.
 * @author Gianluca Amato <gamato@unich.it>
 */
class NumericExpressionSuite extends AnyFunSuite with MockitoSugar with TableDrivenPropertyChecks {

  test("toString with mocks") {
    assertResult("?") { NonDeterministicExpression.toString }

    val lf = mock[LinearForm]
    //when(lf.toDouble) thenReturn (lf)
    when(lf.mkString(any())) thenReturn ("lf")
    assertResult("lf") { LinearExpression(lf).toString }

    val exp1 = mock[NumericExpression]
    val exp2 = mock[NumericExpression]
    when(exp1.mkString(any())) thenReturn ("exp1")
    when(exp2.mkString(any())) thenReturn ("exp2")

    assertResult("(- exp1)") { UnaryMinusExpression(exp1).toString }
    assertResult("(exp1 * exp2)") { MulExpression(exp1, exp2).toString }
    assertResult("(exp1 / exp2)") { DivExpression(exp1, exp2).toString }
    assertResult("(exp1 + exp2)") { AddExpression(exp1, exp2).toString }
    assertResult("(exp1 - exp2)") { SubExpression(exp1, exp2).toString }
  }

  test("Operators on generic expression with mocks") {
    val exp1 = mock[NumericExpression](Mockito.CALLS_REAL_METHODS)
    val exp2 = mock[NumericExpression]
    assertResult(UnaryMinusExpression(exp1)) { -exp1 }
    assertResult(AddExpression(exp1, exp2)) { exp1 + exp2 }
    assertResult(SubExpression(exp1, exp2)) { exp1 - exp2 }
    assertResult(MulExpression(exp1, exp2)) { exp1 * exp2 }
    assertResult(DivExpression(exp1, exp2)) { exp1 / exp2 }
  }

  test("Operators on linear expressions with mocks") {
    val lf1 = mock[LinearForm]
    val lf2 = mock[LinearForm]
    val lfadd = mock[LinearForm]
    val lfsub = mock[LinearForm]
    val lfminus = mock[LinearForm]
    val lfmul = mock[LinearForm]
    val lfdiv = mock[LinearForm]
    val lfconst = LinearForm.c(2)

    when(lf1 + lf2) thenReturn lfadd
    when(lf1 - lf2) thenReturn lfsub
    when(lf1 * 2.0) thenReturn lfmul
    when(lf1 / 2.0) thenReturn lfdiv
    when(-lf1) thenReturn lfminus

    val exp = mock[NumericExpression](Mockito.CALLS_REAL_METHODS)
    val le1 = LinearExpression(lf1)
    val le2 = LinearExpression(lf2)
    val leconst = LinearExpression(lfconst)

    assertResult(LinearExpression(lfadd)) { le1 + le2 }
    assertResult(LinearExpression(lfsub)) { le1 - le2 }
    assertResult(LinearExpression(lfmul)) { le1 * leconst }
    assertResult(LinearExpression(lfmul)) { leconst * le1 }
    assertResult(LinearExpression(lfdiv)) { le1 / leconst }
    assertResult(LinearExpression(lfminus)) { -le1 }

    assertResult(AddExpression(le1, exp)) { le1 + exp }
    assertResult(AddExpression(exp, le1)) { exp + le1 }
    assertResult(SubExpression(le1, exp)) { le1 - exp }
    assertResult(SubExpression(exp, le1)) { exp - le1 }

    assertResult(MulExpression(le1, le2)) { le1 * le2 }
    assertResult(DivExpression(le1, le2)) { le1 / le2 }
    assertResult(DivExpression(leconst, le2)) { leconst / le2 }
  }

  test("Operators without mocks") {
    val v0 = VariableExpression(0)
    val v1 = VariableExpression(1)
    val v2 = VariableExpression(2)
    assertResult(LinearExpression(LinearForm(3, 2, 0, -1))) { 3 + 2 * v0 + v1 - v1 - v2 }
  }

  test("isZero without mocks") {
    val le = LinearExpression(2)
    assert(!le.isZero)

    val le2 = LinearExpression(0)
    assert(le2.isZero)

    val x = VariableExpression(0)
    assert( ! (x*x).isZero )
  }

  test("Methods analyze and assignTo without mocks") {
    val d = BoxDoubleDomain()
    val p = d(Array(-1, 0, 3), Array(1, 2, 3))
    val v0 = VariableExpression(0)
    val v1 = VariableExpression(1)
    val v2 = VariableExpression(2)

    val tests = Table[NumericExpression, Double, Double](
      ("expression", "lower bound", "upper bound"),
      (NonDeterministicExpression, Double.NegativeInfinity, Double.PositiveInfinity),
      (v0 + 2 * v1 + 1, 0, 6),
      (v0 * v2, -3, 3),
      (v0 / v2, -1.0 / 3, 1.0 / 3),
      ((v0 * v2) - (v0 / v2), -3 - 1.0 / 3, 3 + 1.0 / 3))

    forAll(tests) {
      case (expr, lower, upper) =>
        assertResult(d(Array(-1, 0, 3, lower), Array(1, 2, 3, upper))) {
          expr.analyze(p)
        }
    }

    forAll(tests) {
      case (expr, lower, upper) =>
        assertResult(d(Array(lower, 0, 3), Array(upper, 2, 3))) {
          expr.assignTo(0)(p)
        }
    }
  }
}
