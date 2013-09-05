/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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



package it.unich.sci.jandom.targets

import org.scalatest.FunSuite
import it.unich.sci.jandom.domains.numerical.ppl.PPLCPolyhedron
import it.unich.sci.jandom.parsers.NumericalPropertyParser
import it.unich.sci.jandom.targets.jvmsoot.SootFrameNumericalDomain

/**
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class SootFrameNumericalDomainSuite extends FunSuite {
  val numdom = PPLCPolyhedron
  val dom = new SootFrameNumericalDomain(numdom)
  val classAType = soot.RefType.v("classA")
  val types = Seq(classAType, soot.IntType.v(), soot.DoubleType.v())

  test("Bottom and Top") {
    val bottom = dom.bottom(types)
    val top = dom.top(types)
    assert(bottom < top)
  }

  test("Constructors") {
    val env = Environment()
    val parser = new NumericalPropertyParser(env)
    val prop = parser.parseProperty("v0 == v0 && v1 + v2 == 0 && v1 <= 4", dom.numdom).get
    val absframe = dom(prop, types)
    intercept[AssertionError] { dom(prop, Seq(soot.IntType.v(), classAType, soot.DoubleType.v())) }
    intercept[AssertionError] { dom(prop, Seq(soot.IntType.v(), soot.DoubleType.v(), classAType)) }
    intercept[AssertionError] { dom(prop, Seq(classAType, soot.IntType.v())) }
    val fullnumframe = dom(prop, soot.IntType.v())
    intercept[AssertionError] { dom(prop, classAType) }
  }

  test("Connect") {
    val env = Environment()
    val parser = new NumericalPropertyParser(env)
    val prop = parser.parseProperty("v0 == v0 && v1 + v2 == 0 && v1 <= 4", dom.numdom).get
    val prop2 = parser.parseProperty("v0 >= 0 && v1 == v0 && v2 == v2", dom.numdom).get
    val absframe = dom(prop, types)
    val absframe2 = dom(prop2, Seq(soot.DoubleType.v(), soot.DoubleType.v(), classAType))
    val conn = absframe.connect(absframe2, 1)
    val prop3 = parser.parseProperty("v0 == v0 && v1 + v2 == 0 && v1 <=4 && v2 >= 0 && v3 == v3", dom.numdom).get
    val absframe3 = dom(prop3, Seq(classAType, soot.IntType.v(), soot.DoubleType.v(), classAType))
    assert(conn === absframe3)
  }

  test("Restrict") {
    // TODO change the parser API.. is very cumbersome to use since it permanently modifies the environment
    val env = Environment()
    val parser = new NumericalPropertyParser(env)
    val prop = parser.parseProperty("v0 == v0 && v1 + v2 == 0 && v1 <= 4", dom.numdom).get
    val absframe = dom(prop, types)
    val restr = absframe.restrict(2)
    val env2 = Environment()
    val parser2 = new NumericalPropertyParser(env2)
    val prop2 = parser2.parseProperty("v1 + v2 == 0 && v1 <= 4", dom.numdom).get
    val absframe2 = dom(prop2, Seq(soot.IntType.v(), soot.DoubleType.v()))
    assert(restr === absframe2)
  }
}
