/**
  * Copyright 2014, 2016, 2018 Gianluca Amato
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

package it.unich.jandom.parsers

import java.io.{File, FileReader}

import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.targets.NumericCondition._
import it.unich.jandom.targets.NumericExpression._
import it.unich.jandom.targets.lts._
import it.unich.jandom.targets.{Environment, NumericAssignment}
import org.scalatest.funsuite.AnyFunSuite

/**
  * Test suite for `it.unich.jandom.parsers.Fastparser`.
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  *
  */
class FastParserSuite extends AnyFunSuite {

  val dir = new File(getClass.getResource("/fast/").toURI)
  for (model <- dir.listFiles()) {
    test(s"parsing file $model") {
      val source = new FileReader(model)
      val result = FastParser().parse(source)
      source.close()
      if (result.isEmpty) fail(result.toString)
    }
  }

  test("check correct parsing for a simple model") {
    val env = Environment("x", "y")
    val x = VariableExpression(env("x"))
    val y = VariableExpression(env("y"))
    val l1 = Location("start", Nil)
    val l2 = Location("ciclo", Nil)
    val l3 = Location("sink", Nil)
    val t1 = Transition("init", l1, l2,
      guard = Seq(TrueCond),
      assignments = NumericAssignment(0, 0))
    val t2 = Transition("loop", l2, l2,
      guard = List(AtomicCond(LinearForm(-10, 1), ComparisonOperators.LTE)),
      assignments = NumericAssignment(0, LinearForm(1, 1)))
    val t3 = Transition("sink", l2, l3,
      guard = List(AtomicCond(x * x - y, ComparisonOperators.NEQ)),
      assignments = NumericAssignment(0, 0))
    val bad = Region("bad", None, AtomicCond(y, ComparisonOperators.EQ, 3))
    val init = Region("init", Some(l1), AtomicCond(x, ComparisonOperators.EQ))

    val lts = LTS("example", IndexedSeq(l1, l2, l3), Seq(t1, t2, t3), env, Seq(init, bad))

    val fastString =
      """
      model example {
         var x, y;
         states start, ciclo, sink;

         transition init := {
           from := start;
           to := ciclo;
           guard := TRUE;
           action := x' = 0;
         };

         transition loop := {
           from := ciclo;
           to := ciclo;
           guard := x <= 10;
           action := x' = x+1;
         };

        transition sink := {
           from := ciclo;
           to := sink;
           guard := x*x - y != 0;
           action := x'=0;
         };
      }

      strategy s {
         Region init := { state = start && x = 0 };
         Region bad := { y = 3 };
      }
    """
    assert(lts syntacticallyEquals FastParser().parse(fastString).get)
  }
}
