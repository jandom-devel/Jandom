/**
 * Copyright 2017 Gianluca Amato
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

package it.unich.jandom.domains.numerical

import scala.language.implicitConversions

import it.unich.jandom.domains.EmptyExistsSuite
import it.unich.jandom.domains.SeparatedTopAndBottomSuite
import it.unich.jandom.utils.numberext.RationalExt
import it.unich.jandom.utils.numberext.DenseMatrix
import it.unich.jandom.utils.numberext.Bounds
import spire.syntax.literals._

/**
 * Test suite for the parallelotope domain over rationals.
 * @author Gianluca Amato <gamato@unich.it>
 */
class ParallelotopeRationalDomainFastSuite extends NumericalDomainSuite with SeparatedTopAndBottomSuite with EmptyExistsSuite {
  lazy val dom = ParallelotopeRationalDomain()

  val box = dom(Bounds(r"-1",r"-1"), DenseMatrix.eye(2), Bounds(r"1", r"1"))
  val diamond = dom(Bounds(r"-1", r"-1"), DenseMatrix((r"1", r"1"), (r"1", r"-1")), Bounds(r"1", r"1"))
  val empty = dom.bottom(2)
  val full = dom.top(2)

  implicit def r(x: Int) = RationalExt(x)

  override lazy val someProperties = Table("property", dom.bottom(0), dom.bottom(1), dom.bottom(2), dom.bottom(3), dom.bottom(4), dom.bottom(4),
    dom.top(0), dom.top(1), dom.top(2), dom.top(3), dom.top(4), dom.top(5), box, diamond)

  describe("constructors") {
    they("should only work with compatible sizes of bounds and shapes") {
      intercept[IllegalArgumentException] { dom(Bounds(r"0", r"2"), DenseMatrix.eye(2), Bounds(r"0", r"2", r"3")) }
    }
  }

  describe("constructors and extractors for non-trivial parallelotopes") {
    they("should behave as expected") {
      assertResult(2) { box.dimension }
      assertResult(false) { box.isEmpty }
      assertResult(false) { box.isTop }
    }
  }

  describe("constructors and extractors for full parallelotopes") {
    they("should behave as expected") {
      assertResult(2) { full.dimension }
      assertResult(false) { full.isEmpty }
      assertResult(true) { full.isTop }
    }
  }

  describe("constructors and extractors for empty parallelotopes") {
    they("should behave as expected") {
      assertResult(2) { empty.dimension }
      assertResult(true) { empty.isEmpty }
      assertResult(false) { empty.isTop }
    }
  }

  describe("comparison of parallelotopes") {
    it("should behave as expected") {
      assert(empty < box)
      assert(box < full)
      assert(empty < full)
      assert(diamond < box)
      assert(diamond <= box)
      assert(box > diamond)
      assert(box >= diamond)
      assertResult(Some(1)) { box.tryCompareTo(diamond) }
      assertResult(Some(-1)) { diamond.tryCompareTo(box) }
      assert(box == box)
      assertResult(Some(0)) { box.tryCompareTo(box) }
      val box2 = dom(Bounds(r"-1/2", r"-1/2"), DenseMatrix.eye(2), Bounds(r"1/2", r"1/2"))
      assert(box2 <= box)
      assert(box >= box2)
      assert(box2 < box)
      assert(box > box2)
      val box3 = dom(Bounds(r"0", r"0"), DenseMatrix.eye(2), Bounds(r"2", r"2"))
      assertResult(None) { box.tryCompareTo(box3) }
    }
  }

  describe("rotation of shapes") {
    it("should behave as expected") {
      val m = DenseMatrix((r"1", r"1"), (r"-1", r"1"))
      val m1 = DenseMatrix.zeros(m.rows, m.cols)
      for (i <- 0 until m.rows) { for (j <- 0 until m.cols) { m1(i, j) = m(i, j) } }
      val protcalc = box.rotate(m1)
      val protdef = dom(Bounds(r"-2", r"-2"), m, Bounds(r"2", r"2"))
      assertResult(protdef) { protcalc }
    }
  }

  describe("linear invertible assignment") {
    they("should behave as expected") {
      val li1 = dom(Bounds(r"0", r"-1"), DenseMatrix((r"1", -r"1"), (r"0", r"1")), Bounds(r"2", r"1"))
      assertResult(li1) { box.linearAssignment(0, LinearForm(1, 1, 1)) }
      val li2 = dom(Bounds(r"1", r"-1"), DenseMatrix((r"1", r"0"), (r"-1", r"1")), Bounds(r"1", r"0"))
      val li3 = dom(Bounds(r"2", r"-2"), DenseMatrix((r"1", r"0"), (r"-1", r"1")), Bounds(r"2", r"-1"))
      assertResult(li3) { li2.linearAssignment(0, LinearForm(1, 1, 0)) }
      assertResult(li3) { li2.linearAssignment(0, LinearForm(1, 1)) }
      val li4 = dom(Bounds(r"-1", r"-2"), DenseMatrix((r"1", r"0"), (r"-1", r"1")), Bounds(r"1", r"2"))
      assertResult(li4) { box.linearAssignment(1, LinearForm(0, 1, 2)) }
      assert(empty.linearAssignment(1, LinearForm(0, 1, 1)).isEmpty)
    }
  }

  describe("non-invertible linear assignment") {
    they("should behave as expected") {
      val ln1 = dom(Bounds(r"2", r"-1"), DenseMatrix((r"1", r"-1"), (r"0", r"1")), Bounds(r"2", r"1"))
      assertResult(ln1) { box.linearAssignment(0, LinearForm(2, 0, 1)) }
      val ln2 = dom(Bounds(r"0", RationalExt.NegativeInfinity), DenseMatrix((r"-1", r"1"), (r"0", r"1")), Bounds(r"0", RationalExt.PositiveInfinity))
      val ln3 = dom(Bounds(RationalExt.NegativeInfinity, r"0"), DenseMatrix((r"1", r"-1"), (r"0", r"1")), Bounds(RationalExt.PositiveInfinity, r"0"))
      assertResult(ln2) { ln3.linearAssignment(1, LinearForm(0, 1, 0)) }
      assertResult(ln2) { ln3.linearAssignment(1, LinearForm(0, 1)) }
      assert(empty.linearAssignment(1, LinearForm(0, 1, 0)).isEmpty)
    }
  }

  describe("non-deterministic assignment") {
    they("should behave as expected") {
      val nd1 = dom(Bounds(RationalExt.NegativeInfinity, r"-1"), DenseMatrix.eye(2), Bounds(RationalExt.PositiveInfinity, r"1"))
      assert(nd1<=box.nonDeterministicAssignment(0))
      assert(nd1>=box.nonDeterministicAssignment(0))
      assertResult(nd1) { box.nonDeterministicAssignment(0) }
      assertResult(nd1) { nd1.nonDeterministicAssignment(0) }
      assertResult(nd1) { diamond.nonDeterministicAssignment(0) }
      val nd2 = dom(Bounds(r"0", r"0"), DenseMatrix((r"2", r"1"), (r"2", -r"1")), Bounds(r"1", r"1"))
      val nd3 = dom(Bounds(RationalExt.NegativeInfinity, r"-1"), DenseMatrix((r"2", r"1"), (r"0", r"-2")), Bounds(RationalExt.PositiveInfinity, r"1"))
      assertResult(nd3) { nd2.nonDeterministicAssignment(0) }
      val nd4 = dom(Bounds(RationalExt.NegativeInfinity, r"0"), DenseMatrix((r"2", r"1"), (r"4", r"0")), Bounds(RationalExt.PositiveInfinity, r"2"))
      assertResult(nd4) { nd2.nonDeterministicAssignment(1) }
      val nd5 = dom(Bounds(r"10", r"-1"), DenseMatrix((r"1", r"0"), (r"1", r"1")), Bounds(r"10", r"1"))
      val nd6 = dom(Bounds(RationalExt.NegativeInfinity, r"-11"), DenseMatrix.eye(2), Bounds(RationalExt.PositiveInfinity, r"-9"))
      assertResult(nd6) { nd5.nonDeterministicAssignment(0) }
      assert(empty.nonDeterministicAssignment(0).isEmpty)
    }
  }

  describe("linear inequalities") {
    they("should behave as expected") {
      val li1 = dom(Bounds(r"-1", r"-1"), DenseMatrix((r"1", r"1"), (r"1", r"-1")), Bounds(r"0", r"0"))
      assertResult(li1) { diamond.linearInequality(LinearForm(1, 2, 0)) }
      assertResult(li1) { diamond.linearInequality(LinearForm(1, 2)) }
      assert(empty.linearInequality(LinearForm(-1, 1, 0)).isEmpty)
    }
  }

  describe("linear disequalities") {
    they("should behave as expected") {
      val li1 = dom(Bounds(r"-1", r"0"), DenseMatrix((r"1",r"1"),(r"1",r"-2")), Bounds(r"0", r"0"))
      assertResult(li1) { li1.linearDisequality(1) }
      assertResult(empty) { li1.linearDisequality(0) }
      assertResult(li1) { li1.linearDisequality(LinearForm(1, 0, 1)) }
      assertResult(li1) { li1.linearDisequality(LinearForm(0.5, 1, -2)) }
      assertResult(empty) { li1.linearDisequality(LinearForm(0, 1, -2)) }
    }
  }

  describe("union") {
    it("should behave as expected") {
      val u1 = dom(Bounds(2, 0), DenseMatrix.eye(2), Bounds(4, 2))
      val u2 = dom(Bounds(-4, -1), DenseMatrix((r"-1",r"3"), (r"0",r"1")), Bounds(4, 2))
      assertResult(u2) { box union u1 }
      val u3 = dom(Bounds(-1, -1), DenseMatrix((r"0",r"1"), (r"1", r"-1")), Bounds(2, 4))
      assertResult(u3) { u1 union diamond }
      val u4 = dom(Bounds(-4, 0), DenseMatrix.eye(2), Bounds(-2, 2))
      val u5 = dom(Bounds(-4, 0), DenseMatrix.eye(2), Bounds(4, 2))
      assertResult(u5) { u4 union u1 }
      val u6 = dom(Bounds(1, RationalExt.NegativeInfinity), DenseMatrix((r"1", r"0"), (r"1", r"-1")), Bounds(1, 1))
      val u7 = dom(Bounds(0, RationalExt.NegativeInfinity), DenseMatrix((r"1", r"0"), (r"0", r"-1")), Bounds(0, 0))
      val u8 = dom(Bounds(0, 0), DenseMatrix.eye(2), Bounds(1, RationalExt.PositiveInfinity))
      assertResult(u8) { u6 union u7 }
      val u9 = dom(Bounds(0, 0), DenseMatrix.eye(2), Bounds(0, RationalExt.PositiveInfinity))
      assertResult(u8) { u9 union u8 }
      assertResult(u8) { u8 union u9 }
      val u10 = dom(Bounds(2, 0), DenseMatrix.eye(2), Bounds(2, 0))
      val u11 = dom(Bounds(0, 2), DenseMatrix((r"0", r"1"), (r"1", r"-2")), Bounds(1, 6))
      assertResult(u11) { u10 union u11 }
    }
  }

  describe("minimization, maximization and frequency methods") {
    they("should behave as expected") {
      val i = dom(Bounds(-4, -1, 0), DenseMatrix((r"-1", r"3", r"0"), (r"0", r"1", r"0"), (r"-1", r"-1", r"1")), Bounds(4, 2, 0))
      assertResult(12)(i.maximize(LinearForm(0, 1, 1, 0)))
      assertResult(-8)(i.minimize(LinearForm(0, 1, 1, 0)))
      assertResult(None)(i.frequency(LinearForm(0, 1, 1, 0)))
      assertResult(Some(0), i)(i.frequency(LinearForm(0, -1, -1, 1)))
    }
  }

  describe("dimensional variation") {
    it("should behave as expected") {
      val i = diamond
      val j = dom(Bounds(-1, -1, RationalExt.NegativeInfinity), DenseMatrix((r"1",r"1",r"0"),
        (r"1",r"-1",r"0"), (r"0",r"0",r"1")), Bounds(1, 1, RationalExt.PositiveInfinity))
      val h = dom(Bounds(-1, RationalExt.NegativeInfinity), DenseMatrix((r"1", r"0"),
        (r"0",r"1")), Bounds(1, RationalExt.PositiveInfinity))
      assertResult(j)(i.addVariable())
      assertResult(h)(j.delVariable(0))
      assertResult(h)(j.delVariable(1))
      assertResult(i)(j.delVariable(2))
    }
  }

  describe("dimensional maps") {
    they("should behave as expected") {
      val i = diamond
      val h = dom(Bounds(-1), DenseMatrix.eye(1), Bounds(1))
      assertResult(diamond)(diamond.mapVariables(Seq(1, 0)))
      assertResult(diamond)(i.mapVariables(Seq(0, 1)))
      assertResult(h)(i.mapVariables(Seq(-1, 0)))
      assertResult(diamond)(diamond.addVariable().mapVariables(Seq(1, 0, -1)))
    }
  }

  describe("string representation") {
    they("should behave as expected") {
      assertResult("[ -1 ≤ x+y ≤ 1 , -1 ≤ x-y ≤ 1 ]") { diamond.mkString(Seq("x", "y")) }
      assertResult("empty") { empty.toString }
      assertResult("[ -∞ ≤ v0 ≤ +∞ , -∞ ≤ v1 ≤ +∞ ]", full.toString) { full.toString }
    }
  }

  describe("all parallelotopes") {
    they("are polyehdral") {
      forAll(someProperties) { (p) => assert(p.isPolyhedral) }
    }
    they("may be rebuilt from constraints") {
      forAll(someProperties) { (p) =>
        assertResult(p) { p.constraints.foldLeft(p.top) { (prop, lf) => prop.linearInequality(lf) } }
      }
    }
  }
}
