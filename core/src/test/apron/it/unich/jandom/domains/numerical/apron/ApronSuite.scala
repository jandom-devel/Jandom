/**
  * Copyright 2018 Tobia Tesan
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


package it.unich.jandom.domains.numerical.apron
import it.unich.jandom.domains.numerical._
import org.scalatest.FunSuite

class ApronSuite extends FunSuite {
  val dom = ApronIntOctagonDomain()

  test ("linearAssignment sanity check") {
    val top = dom.top(4)
    val lf = new DenseLinearForm(Seq(1,1,0,0,0))
    assert (lf.known == 1)
    val foo = top.linearInequality(lf)
    assert(foo.constraints.toSet == Seq(lf).toSet)
  }

  test ("linearInequality -1 sanity check") {
    val top = dom.top(4)
    val lf = new DenseLinearForm(Seq(-1,1,0,0,0))
    assert (lf.known == -1)
    val foo = top.linearInequality(lf)
    assert(foo.constraints.toSet == Seq(lf).toSet)
  }

  test ("linearInequality -2 sanity check") {
    val top = dom.top(4)
    val lf = new DenseLinearForm(Seq(2,-1,0,0,0))
    assert (lf.known == 2)
    val foo = top.linearInequality(lf)
    assert(foo.constraints.toSet == Seq(lf).toSet)
  }
}
