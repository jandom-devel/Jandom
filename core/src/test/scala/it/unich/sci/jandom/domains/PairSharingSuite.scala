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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM. If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.sci.jandom.domains

import it.unich.sci.jandom.domains.objects._

import org.scalatest.FunSuite

/**
 * A test suite for PairSharing domain.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class PairSharingSuite extends FunSuite {
  val size = 3
  val dom = PairSharingDomain

  def pairs(vars: Seq[Int]) = for (i <- vars; j <- vars) yield UP(i, j)

  test("Bottom element") {
    assert(dom(Set(), size) === dom.bottom(size))
  }

  test("Top element") {
    assert(dom(Set(pairs(0 until size): _*), size) === dom.top(size))
  }

  test("All pairs") {
    assert(dom.top(size) === dom.allPairs(0 until size, size))
    assert(dom.allPairs(Seq(0, 2), 3) === dom(Set(UP(0, 0), UP(0, 2), UP(2, 2)), 3))
  }

  test("Operations on variables") {
    val ps1 = dom.bottom(3)
    val ps2 = ps1.addFreshVariable
    assert(ps2 === dom(Set(UP(3, 3)), 4))
    val ps3 = ps2.assignNull()
    assert(ps3 === dom(Set(), 4))
    val ps4 = ps3.addFreshVariable.assignVariable(0, 4)
    assert(ps4 === dom(Set(UP(4, 4), UP(4, 0), UP(0, 0)), 5))
    val ps5 = ps4.delVariable()
    assert(ps5 === dom(Set(UP(0, 0)), 4))
    val ps6 = ps4.assignVariable(0, 2)
    assert(ps6 === dom(Set(UP(4, 4)), 5))
    val ps7 = ps4.assignVariable(2, 0)
    assert(ps7 === dom.allPairs(Seq(0, 2, 4), 5))
  }

  test("Operations on fields") {
    val ps1 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(3, 3), UP(4, 4), UP(4, 5), UP(5, 5)), 6)
    val ps2 = ps1.assignVariableToField(0, 1, ps1.dimension - 1).delVariable()
    assert(ps2 === dom(Set(UP(0, 0), UP(0, 1), UP(0, 4), UP(1, 1), UP(1, 4), UP(3, 3), UP(4, 4)), 5))
    val ps3 = ps1.assignVariableToField(2, 1, ps1.dimension - 1)
    assert(ps3 === dom.bottom(6))
    val ps4 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(2, 2)), 4)
    val ps5 = ps4.assignVariableToField(0, 1, ps4.dimension - 1).delVariable()
    assert(ps5 == dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(2, 2)), 3))
    assert(ps4.addFreshVariable.assignFieldToVariable(3, 2, 1) == ps4.addFreshVariable.assignVariable(3, 2))
  }

  test("removeHigherVariables") {
    val ps1 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(3, 1), UP(3, 3)), 4)
    val ps2 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1)), 2)
    expectResult(ps2)(ps1.removeHigherVariables(2))
  }

  test("removeLowerVariables") {
    val ps1 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(3, 1), UP(3, 3)), 4)
    val ps2 = dom(Set(UP(1, 1)), 2)
    expectResult(ps2)(ps1.removeLowerVariables(2))
  }

  test("removeRangeOfVariables") {
    val ps1 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(3, 1), UP(3, 3)), 4)
    expectResult( dom(Set(UP(0, 0), UP(1, 1)) ,2) ) (ps1.removeRangeOfVariables(1 to 2))
  }

  test("connectFull: nullness of first property is definitive") {
    val ps1 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(1, 3), UP(3, 3)), 4)
    val ps2 = dom(Set(UP(0, 1), UP(0, 0), UP(1, 1), UP(2, 2), UP(1, 3), UP(3, 3)), 4)
    assert(ps1.connectFull(ps2, 2).isNull(2))
  }

  test("connectFull: nullness of second property is definitive") {
    val ps1 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(1, 3), UP(3, 3)), 4)
    val ps2 = dom(Set(UP(0, 0), UP(2, 2)), 4)
    assert(ps1.connectFull(ps2, 2).isNull(3))
  }

  test("connectFull: connecting from first to second property") {
    val ps1 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(1, 3), UP(3, 3)), 4)
    val ps2 = dom(Set(UP(0, 1), UP(0, 0), UP(1, 1), UP(2, 2), UP(1, 3), UP(3, 3)), 4)
    assert(ps1.connectFull(ps2, 2).ps contains UP(1, 5))
  }

  test("connectFull: connecting two pairs from the first property trough a var") {
    val ps1 = dom(Set(UP(0, 0), UP(1, 1), UP(2, 2), UP(0, 3), UP(1, 3), UP(2, 4), UP(3, 3), UP(4, 4)), 5)
    val ps2 = dom(Set(UP(0, 0)), 4)
    val ps3 = ps1.connectFull(ps2, 2)
    assert(ps3.ps contains UP(0, 1))
    assert(!(ps3.ps contains UP(0, 2)))
    assert(!(ps3.ps contains UP(1, 2)))
  }

  test("connectFull: connecting two pairs from the first property trough a pair") {
    val ps1 = dom(Set(UP(0, 0), UP(1, 1), UP(2, 2), UP(0, 3), UP(1, 3), UP(2, 4), UP(3, 3), UP(4, 4)), 5)
    val ps2 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1)), 4)
    val ps3 = ps1.connectFull(ps2, 2)
    assert(ps3.ps contains UP(0, 1))
    assert(ps3.ps contains UP(0, 2))
    assert(ps3.ps contains UP(1, 2))
  }

  test("connectFull: complex example 1") {
    val ps1 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(1, 3), UP(3, 3)), 4)
    val ps2 = dom(Set(UP(0, 1), UP(0, 0), UP(1, 1), UP(2, 2), UP(1, 3), UP(3, 3)), 4)
    val ps3 = dom(Set(UP(0, 0), UP(0, 1), UP(1, 1), UP(1, 3), UP(1, 5), UP(3, 3), UP(3, 5), UP(4, 4), UP(5, 5)), 6)
    expectResult(ps3)(ps1.connectFull(ps2, 2))
  }

}
