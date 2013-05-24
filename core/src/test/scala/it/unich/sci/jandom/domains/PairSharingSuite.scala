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

import org.scalatest.FunSuite
import it.unich.sci.jandom.domains.objects.PairSharingDomain
import it.unich.sci.jandom.domains.objects.PairSharingProperty
import it.unich.sci.jandom.domains.objects.PairSharingProperty
import it.unich.sci.jandom.domains.objects.UP
import it.unich.sci.jandom.domains.objects.PairSharingProperty

/**
 * A test suite for PairSharing domain.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class PairSharingSuite extends FunSuite {
  val dom = new PairSharingDomain()
  val size = 3
  def types(size: Int) = Seq.fill(size)(new soot.Singletons().soot_RefType())

  test("Bottom element") {
    assert ( PairSharingProperty(Set(), size) === dom.bottom(types(size)) )
  }

  test("Top element") {
    val pairs = for ( i <- 0 until size; j <- i until size ) yield UP(i,j)
	assert ( PairSharingProperty(Set(pairs: _*), size) === dom.top(types(size)) )
  }

  test("Initial element is the same as bottom element") {
    assert (dom.initial(types(size)) === dom.bottom(types(size)))
  }

  test("Complex operations on variables") {
    val ps1 = dom.initial(types(size))
    val ps2 = ps1.evalNull
    assert ( ps2 === PairSharingProperty(Set(), 4) )
    val ps3 = ps2.evalNew
    assert ( ps3 === PairSharingProperty(Set(UP(4,4)), 5) )
    val ps4 = ps3.assignVariable(0)
    assert ( ps4 === PairSharingProperty(Set(UP(0,0)), 4) )
    val ps5 = ps4.evalVariable(0)
    assert ( ps5 === PairSharingProperty(Set(UP(0,0),UP(0,4),UP(4,4)), 5) )
    val ps6 = ps5.evalNull
    assert ( ps6 === PairSharingProperty(Set(UP(0,0),UP(0,4),UP(4,4)), 6) )
    val ps7 = ps6.assignVariable(4)
    assert ( ps7 === PairSharingProperty(Set(UP(0,0)), 5) )
  }

  test("Complex operations of fields") {
	val ps1 = PairSharingProperty(Set(UP(0,0), UP(0,1), UP(1,1),UP(3,3), UP(4,4), UP(4,5), UP(5,5)), 6)
    val ps2 = ps1.assignField(0, 1)
    assert (ps2 === PairSharingProperty(Set(UP(0,0), UP(0,1), UP(0,4), UP(1,1), UP(1,4), UP(3,3), UP(4,4)), 5))
    val ps3 = ps1.assignField(2, 1)
    assert (ps3 === dom.bottom(types(5)))
    val ps4 = PairSharingProperty(Set(UP(0,0), UP(0,1), UP(1,1), UP(2,2)), 4)
    val ps5 = ps4.assignField(0,1)
    assert (ps5 == PairSharingProperty(Set(UP(0,0), UP(0,1), UP(1,1), UP(2,2)), 3))
    assert (ps4.evalField(3,1) == dom.bottom(types(5)))
    assert (ps4.evalField(2,2) == ps4.evalVariable(2))
  }

}