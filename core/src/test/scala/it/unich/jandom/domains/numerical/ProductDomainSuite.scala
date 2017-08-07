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

package it.unich.jandom.domains.numerical

/**
 * Test suite for products of numerical domains.
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
class ProductDomainSuite extends NumericalDomainSuite {
 
  val dom = new ProductDomain(BoxDoubleDomain(), ParallelotopeRationalDomain())
  
  val n = 2

  val empty = dom.bottom(n)
  val full = dom.top(n)
  val boxEmpty = dom.dom1.bottom(n)
  val ptopeFull = dom.dom2.top(n)

  describe("constructors and extractors for the empty pair") {
    assertResult(n) { empty.dimension }
    assertResult(true) { empty.isEmpty }
    assertResult(false) { empty.isTop }
  }

  describe("constructors and extractors for the full pair") {
    assertResult(n) { full.dimension }
    assertResult(false) { full.isEmpty }
    assertResult(true) { full.isTop }
  }

  describe("create a full pair") {
    val p1 = new dom.Property(dom.dom1.top(n), dom.dom2.top(n))
    assertResult(true) { p1.isTop }
    assertResult(false) { p1.isEmpty }
  }

  describe("create a non-empty non-full pair") {
    val box = dom.dom1(Array(1, 2), Array(5, 4))
    val p2 = new dom.Property(box, dom.dom2.top(n))
    assertResult(false) { p2.isTop }
    assertResult(false) { p2.isEmpty }
  }

  describe("create an empty pair") {
    val p3 = new dom.Property(boxEmpty, ptopeFull)
    assertResult(false) { p3.isTop }
    assertResult(true) { p3.isEmpty }
  }

  describe("assignment on product") {
    val x2 = full.linearAssignment(0, 0)
    assertResult(x2) {
      new dom.Property(
        dom.dom1.top(2).linearAssignment(0, 0),
        ptopeFull.linearAssignment(0, 0))
    }
  }

  describe("dimension on product") {
    val x2 = full.linearAssignment(0, 0)
    assertResult(3) { x2.addVariable().dimension }
    assertResult(n + 1) { full.addVariable().dimension }
  }
}
