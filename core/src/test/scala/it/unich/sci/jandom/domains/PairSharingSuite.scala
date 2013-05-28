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

package it.unich.sci.jandom.domains

import scala.collection.immutable.Stack

import org.scalatest.FunSuite

import it.unich.sci.jandom.domains.objects.UP
import it.unich.sci.jandom.targets.jvmsoot.ClassReachableAnalysis
import it.unich.sci.jandom.targets.jvmsoot.SootFramePairSharingDomain

import soot._
import soot.jimple.internal.JimpleLocal

/**
 * A test suite for PairSharing domain.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class PairSharingSuite extends FunSuite {
  val scene = Scene.v()
  val size = 3
  val locals = for (i <- 0 until size) yield new JimpleLocal("v" + i, new soot.Singletons().soot_RefType())
  val field = new SootField("f",new soot.Singletons().soot_RefType())
  val classAnalysis = new ClassReachableAnalysis(scene)
  val dom = new SootFramePairSharingDomain(scene, classAnalysis, locals)

  test("Bottom element") {
    assert(dom.Property(Set(), Stack()) === dom.bottom(Stack()))
  }

  test("Top element") {
    val pairs = for (i <- 0 until size; j <- i until size) yield UP(i, j)
    assert(dom.Property(Set(pairs: _*), Stack()) === dom.top(Stack()))
  }

  test("Initial element is the same as bottom element") {
    assert( dom.initial === dom.bottom(Stack()) )
  }
/*
  test("Complex operations on locals") {
    val ps1 = dom.initial
    val ps2 = ps1.evalNull
    assert(ps2 === dom.Property(Set(), 4))
    val ps3 = ps2.evalNew(RefType.v("fake")
    assert(ps3 === dom.Property(Set(UP(4, 4)), 5))
    val ps4 = ps3.assignLocal(locals(0))
    assert(ps4 === dom.Property(Set(UP(0, 0)), 4))
    val ps5 = ps4.evalLocal(locals(0))
    assert(ps5 === dom.Property(Set(UP(0, 0), UP(0, 4), UP(4, 4)), 5))
    val ps6 = ps5.evalNull
    assert(ps6 === dom.Property(Set(UP(0, 0), UP(0, 4), UP(4, 4)), 6))
    val ps7 = ps6.assignLocal(locals(0))
    assert(ps7 === dom.Property(Set(UP(4, 4)), 5))
  }

  test("Complex operations of fields") {
    val ps1 = dom.Property(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(3, 3), UP(4, 4), UP(4, 5), UP(5, 5)), 6)
    val ps2 = ps1.assignField(locals(0), field)
    assert(ps2 === dom.Property(Set(UP(0, 0), UP(0, 1), UP(0, 4), UP(1, 1), UP(1, 4), UP(3, 3), UP(4, 4)), 5))
    val ps3 = ps1.assignField(locals(2), field)
    assert(ps3 === dom.bottom(2))
    val ps4 = dom.Property(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(2, 2)), 4)
    val ps5 = ps4.assignField(locals(0), field)
    assert(ps5 == dom.Property(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(2, 2)), 3))
    assert(ps4.evalField(locals(2), field) == ps4.evalLocal(locals(2)))
  }
*/
}