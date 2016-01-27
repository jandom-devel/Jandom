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

import java.io.File
import java.io.FileReader
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
import org.scalatest.FunSuite
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.parsers.FastParser
import it.unich.jandom.targets.NumericCondition._
import it.unich.jandom.targets.lts._
import it.unich.jandom.utils.PMaps._
import it.unich.jandom.fixpoint.structured.StructuredDriver

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

  test("simple LTS analysis with equations") {
    val eqs = LTS1.lts.toEQS(dom)
    val ann = StructuredDriver(dom)(eqs, PMap.empty)
    assertResult(dom(Array(0), Array(11))) { ann(LTS1.l2) }
  }

  test("simple LTS analysis with flow equations") {
    val eqs = LTS1.lts.toEQSFlow(dom)
    val ann = StructuredDriver(dom)(eqs, PMap.empty)
    assertResult(dom(Array(0), Array(11))) { ann(LTS1.l2) }
  }

  val dir = new File(getClass.getResource("/fast/").toURI);
  for (model <- dir.listFiles()) {

    val fr = new FileReader(model)
    val source = new PagedSeqReader(PagedSeq.fromReader(fr))
    val result = FastParser().parse(source)
    fr.close()
    val lts = result.getOrElse(fail(result.toString))

    test(s"compare LTS analsysis for ${lts.name} in file ${model}") {
      val params = new Parameters[LTS] { val domain = dom }
      val ann1 = lts.analyze(params)

      val ann2 = StructuredDriver(dom)(lts.toEQS(dom), PMap.empty)
      for (l <- lts.locations) assert(ann1(l) === ann2(l))

      val ann3 = StructuredDriver(dom)(lts.toEQSFlow(dom), PMap.empty)
      for (l <- lts.locations) assert(ann2(l) === ann3(l))
    }
  }
}
