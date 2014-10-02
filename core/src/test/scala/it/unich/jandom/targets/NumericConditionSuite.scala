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

package it.unich.jandom.targets

import org.scalatest.FunSuite

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.LinearForm

import NumericCondition._

/**
 * Test suite for linear conditions.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class NumericConditionSuite extends FunSuite {
  val BoxDouble = BoxDoubleDomain()
  val env = Environment("x","y")
  val lf1 = LinearForm(-3,1,0)
  val lf2 = LinearForm(-6,1,0)
  val cond1 = AtomicCond(lf1,ComparisonOperators.LTE)
  val cond2 = AtomicCond(lf2,ComparisonOperators.GTE)
  val full = BoxDouble.top(env.size)

  test("atomic conditions") {
    assertResult (  BoxDouble(Array(Double.NegativeInfinity,Double.NegativeInfinity), Array(3,Double.PositiveInfinity)) ) { cond1.analyze(full) }
  }

  test("and/or/not conditions") {
    assertResult ( BoxDouble(Array(3,Double.NegativeInfinity),Array(6,Double.PositiveInfinity)) ) { OrCond(cond1,cond2).opposite.analyze(full) }
    assertResult ( BoxDouble(Array(3,Double.NegativeInfinity),Array(6,Double.PositiveInfinity)) ) { NotCond(OrCond(cond1,cond2)).analyze(full) }
  }
}
