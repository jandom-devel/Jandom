/**
 * Copyright 2013 Gianluca Amato, Francesca Scozzari
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

package it.unich.jandom.domains

import org.scalatest.FunSuite
import breeze.linalg._
import it.unich.jandom.domains.numerical._

/**
 * Test suite for products of numerical domains.
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
class ProductSuite extends FunSuite {

  val n = 2

  val productDomain = new ProductDomain(BoxDoubleDomain(), ParallelotopeDomain())
  val empty = productDomain.bottom(n)
  val full = productDomain.top(n)
  val boxEmpty = productDomain.dom1.bottom(n)
  val ptopeFull = productDomain.dom2.top(n)

  test("constructors and extractors for the empty pair") {
    assertResult(n) { empty.dimension }
    assertResult(true) { empty.isEmpty }
    assertResult(false) { empty.isTop }
  }

  test("constructors and extractors for the full pair") {
    assertResult(n) { full.dimension }
    assertResult(false) { full.isEmpty }
    assertResult(true) { full.isTop }
  }

  test("create a full pair") {
    val p1 = new productDomain.Property(productDomain.dom1.top(n), productDomain.dom2.top(n))
    assertResult(true) { p1.isTop }
    assertResult(false) { p1.isEmpty }
  }

  test("create a non-empty non-full pair") {
    val box = productDomain.dom1(Array(1, 2), Array(5, 4))
    val p2 = new productDomain.Property(box, productDomain.dom2.top(n))
    assertResult(false) { p2.isTop }
    assertResult(false) { p2.isEmpty }
  }

  test("create an empty pair") {
    val p3 = new productDomain.Property(boxEmpty, ptopeFull)
    assertResult(false) { p3.isTop }
    assertResult(true) { p3.isEmpty }
  }

  test("assignment on product") {
    val x2 = full.linearAssignment(0, 0.0)
    assertResult(x2) {
      new productDomain.Property(
        productDomain.dom1.top(2).linearAssignment(0, 0.0),
        ptopeFull.linearAssignment(0, 0.0))
    }
  }

  test("dimension on product") {
    val x2 = full.linearAssignment(0, 0.0)
    assertResult(3) { x2.addVariable().dimension }
    assertResult(n + 1) { full.addVariable().dimension }
  }
}
