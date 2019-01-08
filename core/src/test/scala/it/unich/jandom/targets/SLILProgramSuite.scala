/**
  * Copyright 2013, 2016, 2018 Gianluca Amato <gianluca.amato@unich.it>
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

import NumericCondition._
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.parsers.RandomParser
import it.unich.jandom.targets.parameters.NarrowingStrategy
import it.unich.jandom.targets.parameters.WideningScope
import it.unich.jandom.targets.slil._

/**
  * Test suite for SLIL programs.
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  *
  */
class SLILProgramSuite extends FunSuite {
  val BoxDouble = BoxDoubleDomain()

  test("simple program 1") {
    val env = Environment("x")
    val program = SLILProgram(env, Seq(1),
      CompoundStmt(
        AssignStmt(0, 0),
        WhileStmt(AtomicCond(LinearForm(-10, 1), ComparisonOperators.LT),
          AssignStmt(0, LinearForm(1, 1)))))
    val params = new Parameters[SLILTarget] {
      val domain: SLILTarget#DomainBase = BoxDouble
    }
    val ann = program.analyze(params)
    assertResult(BoxDouble(Array(10), Array(11))) {
      ann((program.stmt, 2))
    }
  }

  test("input vs output widening") {
    val source =
      """
      localwidening = function() {
        i = 0
        while (TRUE) {
              tag(0)
          if (brandom())
             i = 1
          else
             i = -1
            }
      }
    """
    val parsed = RandomParser().parse(source)
    val program = parsed.get

    val params = new Parameters[SLILTarget] {
      val domain: SLILTarget#DomainBase = BoxDouble
    }
    params.narrowingStrategy = NarrowingStrategy.None
    params.wideningScope = WideningScope.Output
    program.analyze(params)
    assertResult(BoxDouble.top(1)) {
      params.tag(0)
    }

    params.narrowingStrategy = NarrowingStrategy.None
    params.wideningScope = WideningScope.Random
    program.analyze(params)
    assertResult(BoxDouble.top(1)) {
      params.tag(0)
    }

    params.narrowingStrategy = NarrowingStrategy.None
    params.wideningScope = WideningScope.BackEdges
    program.analyze(params)
    assertResult(BoxDouble(Array(-1), Array(1))) {
      params.tag(0)
    }
  }

  test("statement without program") {
    val stmt: SLILTarget =
      CompoundStmt(
        AssignStmt(0, 0),
        WhileStmt(AtomicCond(LinearForm(-10, 1), ComparisonOperators.LT),
          AssignStmt(0, LinearForm(1, 1))))
    val params = new Parameters[SLILTarget] {
      val domain: SLILTarget#DomainBase = BoxDouble
    }
    val ann = stmt.analyze(params)
    assertResult(BoxDouble(Array(10), Array(11))) {
      ann((stmt, 2))
    }
  }

  test("check numvars for statements") {
    val stmt: SLILStmt = CompoundStmt ( NopStmt, AssignStmt(0, 0))
    assertResult(1)(stmt.numvars)
  }
}
