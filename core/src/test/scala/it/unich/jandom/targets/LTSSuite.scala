/**
  * Copyright 2013, 2015, 2016, 2018 Gianluca Amato
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

import java.io.File
import java.io.FileReader

import org.scalatest.funsuite.AnyFunSuite
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.parsers.FastParser
import it.unich.jandom.targets.NumericCondition._
import it.unich.jandom.targets.lts._
import it.unich.scalafix.finite.FiniteFixpointSolver
import it.unich.scalafix.FixpointSolver._
import it.unich.scalafix.lattice.Domain

class LTSSuite extends AnyFunSuite {
  val dom = BoxDoubleDomain()

  implicit val scalafixDomain: Domain[dom.Property] = dom.ScalaFixDomain
  private val wideningBox = { (x: dom.Property, y: dom.Property) => x widening y }
  private val narrowingBox = { (x: dom.Property, y: dom.Property) => x narrowing y }
  private val CC77 = FiniteFixpointSolver.CC77[Location, dom.Property](Solver.PriorityWorkListSolver, wideningBox, narrowingBox)

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

  test("dot translation") {
    val output =
      """digraph {
        |  "0" [label="start"]
        |  "1" [label="ciclo"]
        |  "0" -> "1" [label="init"]
        |  "1" -> "1" [label="loop"]
        |}
        |""".stripMargin('|')
    assertResult(output)(LTS1.lts.toDot)
  }

  test("simple LTS analysis") {
    val params = new Parameters[LTS] {
      val domain: LTS#DomainBase = dom
    }
    val ann = LTS1.lts.analyze(params)
    assertResult(dom(Array(0), Array(11))) {
      ann(LTS1.l2)
    }
  }

  test("simple LTS analysis with equations") {
    val eqs = LTS1.lts.toEquationSystem(dom)
    val ann = FiniteFixpointSolver(eqs, CC77)
    assertResult(dom(Array(0), Array(11))) {
      ann(LTS1.l2)
    }
  }

  val dir = new File(getClass.getResource("/fast/").toURI)
  for (model <- dir.listFiles()) {
    val source = new FileReader(model)
    val result = FastParser().parse(source)
    source.close()
    val lts = result.getOrElse(fail(result.toString))

    test(s"compare LTS analsysis for ${lts.name} in file $model") {
      val params = new Parameters[LTS] {
        val domain: LTS#DomainBase = dom
      }
      val ann1 = lts.analyze(params)
      val ann2 = FiniteFixpointSolver(lts.toEquationSystem(dom), CC77)
      for (l <- lts.locations) assert(ann1(l) === ann2(l))
    }
  }
}
