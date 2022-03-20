/**
  * Copyright 2013, 2018 Gianluca Amato
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

package it.unich.jandom.parsers

import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.targets.{Environment, NumericAssignment}
import it.unich.jandom.targets.NumericCondition._
import it.unich.jandom.targets.lts.{LTS, Location, Transition}
import org.scalatest.funsuite.AnyFunSuite

/**
  * Test suite for LPInv Parser.
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  *
  */
class LPInvParserSuite extends AnyFunSuite {
  test("simple LTS") {
    val env = Environment("x")
    val l1 = Location("start", Nil)
    val l2 = Location("ciclo", List(FalseCond))
    val t1 = Transition("init", l1, l2,
      guard = Nil,
      assignments = NumericAssignment(0, LinearForm(0)))
    val t2 = Transition("loop", l2, l2,
      guard = List(AtomicCond(LinearForm(-10, 1), ComparisonOperators.LTE)),
      assignments = NumericAssignment(0, LinearForm(1, 1)))
    val lts = LTS("example", IndexedSeq(l1, l2), Seq(t1, t2), env)

    val ltsString =
      """
	  var x;
	  location start with ( );
	  location ciclo with ( FALSE );
	  transition init start -> ciclo with Guard ()
	    x := 0;
	  transition loop ciclo -> ciclo with Guard ( x<=10 )
	    x := x+1;
	  end
	  """
    assert(lts syntacticallyEquals LPInvParser().parse("example", ltsString).get)
  }
}
