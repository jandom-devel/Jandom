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

package it.unich.sci.jandom.domains

import org.scalatest.FunSuite
import breeze.linalg._
import it.unich.sci.jandom.domains.numerical._

/**
 * Test suite for domain product.
 * @author Francesca Scozzari <fscozzari@unich.it>
 *
 */
class ProductSuite extends FunSuite {

  val d1 = BoxDouble
  val d2 = Parallelotope
  val n=2


  val productDomain = new ProductDomain {
    val dom1 = d1
    val dom2 = d2
  	val dom1Todom2 = implicitly[DomainTransformation[d1.Property, d2.Property]]
  	val dom2Todom1 = implicitly[DomainTransformation[d2.Property, d1.Property]]
  }

  val empty = productDomain.bottom(n)
  val full = productDomain.top(n)

  test("constructors and extractors for the empty pair") {
    expectResult(n) { empty.dimension }
    expectResult(true) { empty.isEmpty }
    expectResult(false) { empty.isTop }
  }

  test("constructors and extractors for the full pair") {
    expectResult(n) { full.dimension }
    expectResult(false) { full.isEmpty }
    expectResult(true) { full.isTop }
  }

  val p1= new productDomain.Property(BoxDouble.top(n), Parallelotope.top(n))
  test("construct a full pair") {
    expectResult(true) {p1.isTop}
    expectResult(false) {p1.isEmpty}
  }

  val box = BoxDouble(Array(1, 2), Array(5, 4))
  val p2= new productDomain.Property(box, Parallelotope.top(n))
 test("construct a non-empty non-full pair") {
    expectResult(false) {p2.isTop}
    expectResult(false) {p2.isEmpty}
  }


  val boxEmpty =  BoxDouble.bottom(n)
  val ptopeFull = Parallelotope.top(n)
  val p3= new productDomain.Property(boxEmpty, ptopeFull)
 test("construct an empty product") {
    expectResult(false) {p3.isTop}
    expectResult(true) {p3.isEmpty}
  }

     // assign v0 = 0
  val x2 = full.linearAssignment(0, 0.0)
  test("assignment on product") {
    expectResult(true) {new productDomain.Property(
        BoxDouble.top(2).linearAssignment(0, 0.0),
        ptopeFull.linearAssignment(0, 0.0)) <=  x2}
      expectResult(true) {new productDomain.Property(
        BoxDouble.top(2).linearAssignment(0, 0.0),
        ptopeFull.linearAssignment(0, 0.0)) >=  x2}
  }

  test("dimension on product") {
    expectResult(3) {x2.addVariable().dimension}
    expectResult(n+1) {full.addVariable().dimension}

  }

}
