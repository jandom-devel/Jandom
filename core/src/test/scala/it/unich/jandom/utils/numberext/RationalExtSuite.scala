/**
 * Copyright 2013, 2016 Gianluca Amato
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

package it.unich.jandom.utils.numberext

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

/**
 * Test suite for extended rationals.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class RationalExtSuite extends FunSuite with PropertyChecks {
  import RationalExt._

  test("equality on infinities") {
    assert(PositiveInfinity == PositiveInfinity, "+Inf = +Inf")
    assert(NegativeInfinity == NegativeInfinity, "-Inf = -Inf")
    assert(PositiveInfinity != NegativeInfinity, "+Inf != -Inf")
    assert(NegativeInfinity != PositiveInfinity, "-Inf != +Inf")
    assert(NegativeInfinity != NaN, "-Inf != NaN")
    assert(PositiveInfinity != NaN, "+Inf != NaN")
    assert(NaN != NaN, "NaN != NaN")
    assert(NaN != NegativeInfinity, "NaN != +Inf")
    assert(NaN != PositiveInfinity, "NaN != +Inf")
    assert(NaN != RationalExt(2), "NaN != 2")
  }

  test("equality on regular numbers") {
    val n1 = RationalExt(2)
    val n2 = RationalExt(2)
    val n3 = RationalExt(3)
    assert(n1 === n1)
    assert(n1 === n2)
    assert(n1 != n3)
    assert(n2 == n1)
    assert(n2 == n2)
    assert(n2 != n3)
    assert(n3 != n1)
    assert(n3 != n2)
    assert(n3 == n3)
  }

  test("inequalities with infinities") {
    assert(PositiveInfinity <= PositiveInfinity, "+Inf <= +Inf")
    assert(! (PositiveInfinity <= NegativeInfinity), "not +Inf <= -Inf")
    assert(NegativeInfinity <= PositiveInfinity, "-Inf <= +Inf")
    assert(NegativeInfinity <= NegativeInfinity, "-Inf <= -Inf")

    assert(! (PositiveInfinity < PositiveInfinity), "not +Inf < +Inf")
    assert(! (PositiveInfinity < NegativeInfinity), "not +Inf < -Inf")
    assert(NegativeInfinity < PositiveInfinity, "-Inf < +Inf")
    assert(! (NegativeInfinity < NegativeInfinity), "not -Inf < -Inf")

    assert(! (NaN <= PositiveInfinity))
    assert(! (NaN < PositiveInfinity))
    assert(! (NaN <= NegativeInfinity))
    assert(! (NaN < NegativeInfinity))
    assert(! (PositiveInfinity <= NaN))
    assert(! (PositiveInfinity < NaN))
    assert(! (NegativeInfinity <= NaN))
    assert(! (NegativeInfinity < NaN))

    forAll { (w: Int)  =>
      val rw  = RationalExt(w)
      assert(rw <= PositiveInfinity)
      assert(rw < PositiveInfinity)
      assert(NegativeInfinity <= rw)
      assert(NegativeInfinity < rw)
    }
  }

  test("binary operations on regular numbers") {
    forAll(Gen.choose(-10000, 10000), Gen.choose(-10000, 10000)) { (w: Int, h: Int) =>
      val rw = RationalExt(w)
      val rh = RationalExt(h)
      assertResult(RationalExt(w + h))(rw + rh)
      assertResult(RationalExt(w - h))(rw - rh)
      assertResult(RationalExt(w * h))(rw * rh)
    }
  }

  test("unary operations in regular numbers") {
    forAll(Gen.choose(Int.MinValue + 1, Int.MaxValue)) { (w: Int) =>
      val rw = RationalExt(w)
      assertResult(rw)(+rw)
      assertResult(RationalExt(-w))(-rw)
      assertResult(RationalExt(Math.abs(w)))(rw.abs)
    }
  }

  test("sum on infinities") {
    forAll { (x: Int) =>
      val rx = RationalExt(x)
      assertResult(PositiveInfinity) { rx + PositiveInfinity }
      assertResult(PositiveInfinity) { PositiveInfinity + rx }
      assertResult(NegativeInfinity) { rx + NegativeInfinity }
      assertResult(NegativeInfinity) { NegativeInfinity + rx }
      assertResult(NaN) { NaN + rx}
      assertResult(NaN) { rx + NaN }
    }
    assertResult(PositiveInfinity) { PositiveInfinity + PositiveInfinity }
    assertResult(NegativeInfinity) { NegativeInfinity + NegativeInfinity }
    assertResult(NaN) { PositiveInfinity + NegativeInfinity }
    assertResult(NaN) { NegativeInfinity + PositiveInfinity }
    assertResult(NaN) { NaN + NaN }
  }

  test("differences on infinities") {
    forAll { (x: Int) =>
      val rx = RationalExt(x)
      assertResult(NegativeInfinity) { rx - PositiveInfinity }
      assertResult(PositiveInfinity) { PositiveInfinity - rx }
      assertResult(PositiveInfinity) { rx - NegativeInfinity }
      assertResult(NegativeInfinity) { NegativeInfinity - rx }
      assertResult(NaN) { NaN - rx }
      assertResult(NaN) { rx - NaN }
    }
    assertResult(NaN) { PositiveInfinity - PositiveInfinity }
    assertResult(NaN) { NegativeInfinity - NegativeInfinity }
    assertResult(PositiveInfinity) { PositiveInfinity - NegativeInfinity }
    assertResult(NegativeInfinity) { NegativeInfinity - PositiveInfinity }
    assertResult(NaN) { NaN - NaN }
  }

  test("mkString method") {
    assertResult("12.34") { RationalExt(1234,100).mkString(2,java.math.RoundingMode.FLOOR) }
    assertResult("12.3") { RationalExt(1234,100).mkString(1,java.math.RoundingMode.FLOOR) }
    assertResult("12.4") { RationalExt(1234,100).mkString(1,java.math.RoundingMode.CEILING) }
  }

  test("toString method") {
    forAll { (w: Int) =>
      assertResult(w.toString)(RationalExt(w).toString)
    }
    assertResult("Infinity") { PositiveInfinity.toString }
    assertResult("-Infinity") { NegativeInfinity.toString }
    assertResult("NaN") { NaN.toString }
  }
}
