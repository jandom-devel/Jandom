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

package it.unich.sci.jandom.targets

import org.scalatest.FunSuite

import it.unich.sci.jandom.domains.BoxDouble

import linearcondition.{FalseCond,AtomicCond}
import lts._

/**
 * Test suite for LTS.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class LTSSuite extends FunSuite {
  test("simple LTS analysis") {
    val env = Environment("x")
	val l1 = Location("start", Nil)
	val l2 = Location("ciclo", List(FalseCond))
	val t1 = Transition("init", l1, l2, 
	    guard = Nil, 
	    assignments = List(LinearAssignment(0,LinearForm.fromCoefficient(0))))
	val t2 = Transition("loop", l2, l2, 
	    guard = List(AtomicCond(LinearForm(List(-10,1)), AtomicCond.ComparisonOperators.LTE)),
	    assignments = List(LinearAssignment(0,LinearForm(List(1,1)))))
	val lts = LTS(IndexedSeq(l1,l2), Seq(t1,t2), env)
	val params = new Parameters(lts) { val domain = BoxDouble }    
    val ann = lts.analyze(params)
    expectResult ( BoxDouble(Array(0), Array(11)) ) { ann(l2) }
  }   
}
