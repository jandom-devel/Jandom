/**
 * Copyright 2013, 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.domains.objects

import org.scalatest.funsuite.AnyFunSuite
import it.unich.jandom.objectmodels.ObjectModel
import it.unich.jandom.objectmodels.NoArrays
import it.unich.jandom.objectmodels.TrivialObjectModel
import it.unich.jandom.objectmodels.ObjectModelHelper

/**
 * A test suite for PairSharing domain.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class PairSharingSuite extends AnyFunSuite {
  import scala.language.implicitConversions

  val dom = PairSharingDomain(TrivialObjectModel)

  implicit def sizeToTypes(size: Int) = Seq.fill(size)(())

  implicit def paitToUP(p: (Int,Int)) = UP(p)

  test("Bottom element") {
    for (size <- 1 to 3) assert(dom(Set(), size) === dom.bottom(size))
  }

  test("Top element") {
    def pairs(vars: Seq[Int]) = for (i <- vars; j <- vars) yield UP(i, j)
    for (size <- 1 to 3) assert(dom(Set(pairs(0 until size): _*), size) === dom.top(size))
  }

  test("All pairs") {
    for (size <- 1 to 3) assert(dom.top(size) === dom.allPairs(0 until size, size))
    assert(dom.allPairs(Seq(0, 2), 3) === dom(Set((0, 0), (0, 2), (2, 2)), 3))
  }

  test("Operations on variables") {
    val ps1 = dom.bottom(3)
    val ps2 = ps1.addFreshVariable(())
    assert(ps2 === dom(Set((3, 3)), 4))
    val ps3 = ps2.assignNull()
    assert(ps3 === dom(Set(), 4))
    val ps4 = ps3.addFreshVariable(()).assignVariable(0, 4)
    assert(ps4 === dom(Set((4, 4), (4, 0), (0, 0)), 5))
    val ps5 = ps4.delVariable()
    assert(ps5 === dom(Set((0, 0)), 4))
    val ps6 = ps4.assignVariable(0, 2)
    assert(ps6 === dom(Set((4, 4)), 5))
    val ps7 = ps4.assignVariable(2, 0)
    assert(ps7 === dom.allPairs(Seq(0, 2, 4), 5))
  }

  test("Operations on fields") {
    val ps1 = dom(Set((0, 0), (0, 1), (1, 1), (3, 3), (4, 4), (4, 5), (5, 5)), 6)
    val ps2 = ps1.assignVariableToField(0, (), ps1.dimension - 1).delVariable()
    assert(ps2 === dom(Set((0, 0), (0, 1), (0, 4), (1, 1), (1, 4), (3, 3), (4, 4)), 5))
    val ps3 = ps1.assignVariableToField(2, (), ps1.dimension - 1)
    assert(ps3 === dom.bottom(6))
    val ps4 = dom(Set((0, 0), (0, 1), (1, 1), (2, 2)), 4)
    val ps5 = ps4.assignVariableToField(0, (), ps4.dimension - 1).delVariable()
    assert(ps5 === dom(Set((0, 0), (0, 1), (1, 1), (2, 2)), 3))
    assert(ps4.addFreshVariable(()).assignFieldToVariable(3, 2, ()) === ps4.addFreshVariable(()).assignVariable(3, 2))
  }

  test("Delete last variables") {
    val ps1 = dom(Set((0, 0), (0, 1), (1, 1), (3, 1), (3, 3)), 4)
    val ps2 = dom(Set((0, 0), (0, 1), (1, 1)), 2)
    assertResult(ps2)(ps1.delVariables(2 until 4))
  }

  test("Delete first variables") {
    val ps1 = dom(Set((0, 0), (0, 1), (1, 1), (3, 1), (3, 3)), 4)
    val ps2 = dom(Set((1, 1)), 2)
    assertResult(ps2)(ps1.delVariables(0 until 2))
  }

  test("Delete variables in the middle") {
    val ps1 = dom(Set((0, 0), (0, 1), (1, 1), (3, 1), (3, 3)), 4)
    assertResult(dom(Set((0, 0), (1, 1)), 2))(ps1.delVariables(1 to 2))
    assertResult(dom(Set((0, 0), (2, 2)), 3))(ps1.delVariable(1))
  }

  test("Map variables") {
    val ps1 = dom(Set((0, 0), (0, 2), (2, 2)), 3)
    val ps2 = ps1.mapVariables(Seq(1, 0, -1))
    assert(ps2 === dom(Set((1, 1)), 2))
  }

  test("connectFull: nullness of first property is definitive") {
    val ps1 = dom(Set((0, 0), (0, 1), (1, 1), (1, 3), (3, 3)), 4)
    val ps2 = dom(Set((0, 1), (0, 0), (1, 1), (2, 2), (1, 3), (3, 3)), 4)
    assert(ps1.connectFull(ps2, 2).mustBeNull(2))
  }

  test("connectFull: nullness of second property is definitive") {
    val ps1 = dom(Set((0, 0), (0, 1), (1, 1), (1, 3), (3, 3)), 4)
    val ps2 = dom(Set((0, 0), (2, 2)), 4)
    assert(ps1.connectFull(ps2, 2).mustBeNull(3))
  }

  test("connectFull: connecting from first to second property") {
    val ps1 = dom(Set((0, 0), (0, 1), (1, 1), (1, 3), (3, 3)), 4)
    val ps2 = dom(Set((0, 1), (0, 0), (1, 1), (2, 2), (1, 3), (3, 3)), 4)
    assert(ps1.connectFull(ps2, 2).ps contains ((1, 5)) )
  }

  test("connectFull: connecting two pairs from the first property trough a var") {
    val ps1 = dom(Set((0, 0), (1, 1), (2, 2), (0, 3), (1, 3), (2, 4), (3, 3), (4, 4)), 5)
    val ps2 = dom(Set((0, 0)), 4)
    val ps3 = ps1.connectFull(ps2, 2)
    assert(ps3.ps contains ((0, 1)) )
    assert(!(ps3.ps contains ((0, 2)) ))
    assert(!(ps3.ps contains ((1, 2)) ))
  }

  test("connectFull: connecting two pairs from the first property trough a pair") {
    val ps1 = dom(Set((0, 0), (1, 1), (2, 2), (0, 3), (1, 3), (2, 4), (3, 3), (4, 4)), 5)
    val ps2 = dom(Set((0, 0), (0, 1), (1, 1)), 4)
    val ps3 = ps1.connectFull(ps2, 2)
    assert(ps3.ps contains ((0, 1)) )
    assert(ps3.ps contains ((0, 2)) )
    assert(ps3.ps contains ((1, 2)) )
  }

  test("connectFull: complex example 1") {
    val ps1 = dom(Set((0, 0), (0, 1), (1, 1), (1, 3), (3, 3)), 4)
    val ps2 = dom(Set((0, 1), (0, 0), (1, 1), (2, 2), (1, 3), (3, 3)), 4)
    val ps3 = dom(Set((0, 0), (0, 1), (1, 1), (1, 3), (1, 5), (3, 3), (3, 5), (4, 4), (5, 5)), 6)
    assertResult(ps3)(ps1.connectFull(ps2, 2))
  }

  test("connectFull: null at the output") {
    val ps1 = dom(Set((0, 0), (0, 1), (1, 1)), 2)
    val ps2 = dom(Set((1, 1), (2, 2)), 3)
    val ps3 = dom(Set((0, 0), (3, 3), (2, 2)), 4)
    assertResult(ps3)(ps1.connectFull(ps2, 1))
  }

  test("A non trivial object model") {
    object NonTrivialModel extends ObjectModel with NoArrays with ObjectModelHelper {
      type Type = Int
      type Field = Int
      def isPrimitive(t: Type) = false
      def isConcrete(t: Type) = true
      def declaredFields(t: Type) = Set()
      def typeOf(f: Field) = f
      def parents(t: Type) = Set()
      def children(t: Type) = Set()
      def lteq(t1: Type, t2: Type) = t1 == t2
      override def mayShare(src: Type, tgt: Type) = UP(src,tgt) != UP(0,1)
      override def mayBeAliases(t1: Type, t2: Type) = t1 == t2
    }

    val dom = new PairSharingDomain(NonTrivialModel)

    assert(dom.top(Seq(0,1,2)) === dom(Set( (0, 0), (1, 1), (1,2), (0,2), (2, 2)), Seq(0,1,2)))
    assert(dom.allPairs(Seq(0, 1), Seq(0,1)) === dom(Set((0, 0), (1, 1)), Seq(0,1)))
    val ps = dom.top(Seq(0,1,2)).addFreshVariable(0)
    assert(ps.assignFieldToVariable(3, 2, 0) === dom(Set((2, 3), (2, 2), (1, 2), (0, 0), (3, 3), (0, 2), (0, 3), (1, 1)),Seq(0, 1, 2,0)))
  }
}
