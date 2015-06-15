/**
 * Copyright 2013, 2015 Gianluca Amato
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

import org.scalatest.FunSuite

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.LinearForm

import NumericCondition._
import lts._

/**
 * Test suite for LTS.
 * @author Gianluca Amato <gamato@unich.it>
 */
class LTSSuite extends FunSuite {
  val dom = BoxDoubleDomain()

  object LTS1 {
    val env = Environment("x")
    val l1 = Location("start", Nil)
    val l2 = Location("ciclo", List(FalseCond))
    val t1 = Transition("init", l1, l2,
      guard = Nil,
      assignments = NumericAssignment(0, 0))
    val t2 = Transition("loop", l2, l2,
      guard = List(AtomicCond(LinearForm(-10, 1), ComparisonOperators.LTE)),
      assignments = NumericAssignment(0, LinearForm(1, 1)))
    val lts = LTS("example", IndexedSeq(l1, l2), Seq(t1, t2), env)
  }

  test("simple LTS analysis") {
    val params = new Parameters[LTS] { val domain = dom }
    val ann = LTS1.lts.analyze(params)
    assertResult(dom(Array(0), Array(11))) { ann(LTS1.l2) }
  }
}
