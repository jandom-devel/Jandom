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

  test("constructors and extractors the empty pair") {
    expectResult(n) { empty.dimension }
    expectResult(true) { empty.isEmpty }
    expectResult(false) { empty.isTop }
  }

  test("constructors and extractors for the full pair") {
    expectResult(n) { full.dimension }
    expectResult(false) { full.isEmpty }
    expectResult(true) { full.isTop }
  }

  /*
  test("constructors should only work with compatible sizes") {
    intercept[IllegalArgumentException] { Parallelotope(DenseVector(0, 2), DenseMatrix.eye(2), DenseVector(0, 2, 3)) }
  }
  */

  val p1= new productDomain.ProductProperty(BoxDouble.top(n), Parallelotope.top(n))
  test("construct a full product") {
    expectResult(true) {p1.isTop}
    expectResult(false) {p1.isEmpty}

  }

  val box = BoxDouble(Array(1, 2), Array(5, 4))
  val p2= new productDomain.ProductProperty(box, Parallelotope.top(n))
 test("construct a full product 2") {
    expectResult(false) {p1.isTop}
    expectResult(false) {p1.isEmpty}
  }


  val boxEmpty =  BoxDouble.bottom(n)
  val ptopeFull = Parallelotope.top(n)
  val p3= new productDomain.ProductProperty(boxEmpty, ptopeFull)
 test("construct an empty product") {
    expectResult(false) {p1.isTop}
    expectResult(true) {p1.isEmpty}
  }

     // assign v0 = 0
  val x2 = full.linearAssignment(0, Array(0,0), 0)
 test("assignment on product") {
    expectResult(new productDomain.ProductProperty(
        BoxDouble.top(n).linearAssignment(0, Array(0,0), 0),
        ptopeFull.linearAssignment(0, Array(0,0), 0))) {x2}
  }

  test("dimension on product") {
    expectResult(n+1) {x2.addVariable().dimension}
  }


/*

  test("comparison of parallelotopes") {
    assert(empty < v2)
    assert(v2 < full)
    assert(empty < full)

    expectResult(Some(0)) { empty.tryCompareTo(empty) }
    expectResult(Some(-1)) { full.tryCompareTo(empty) }
    assert(v2 == v2)
    expectResult(Some(0)) { v2.tryCompareTo(v2) }
  }


  test("linear invertible assignment") {
    val li1 = Parallelotope(DenseVector(0, -1), DenseMatrix((1.0, -1.0), (0.0, 1.0)), DenseVector(2, 1))
    expectResult(li1) { box.linearAssignment(0, Array(1, 1), 1) }
    val li2 = Parallelotope(DenseVector(1, -1), DenseMatrix((1.0, 0.0), (-1.0, 1.0)), DenseVector(1, 0))
    val li3 = Parallelotope(DenseVector(2, -2), DenseMatrix((1.0, 0.0), (-1.0, 1.0)), DenseVector(2, -1))
    expectResult(li3) { li2.linearAssignment(0, Array(1, 0), 1) }
    expectResult(li3) { li2.linearAssignment(0, Array(1), 1) }
    val li4 = Parallelotope(DenseVector(-1, -2), DenseMatrix((1.0, 0.0), (-1.0, 1.0)), DenseVector(1, 2))
    expectResult(li4) { box.linearAssignment(1, Array(1, 2), 0) }
    assert(empty.linearAssignment(1, Array(1, 1), 0).isEmpty)
  }

  test("non-invertible linear assignment") {
    val ln1 = Parallelotope(DenseVector(2, -1), DenseMatrix((1.0, -1.0), (0.0, 1.0)), DenseVector(2, 1))
    expectResult(ln1) { box.linearAssignment(0, Array(0, 1), 2) }
    val ln2 = Parallelotope(DenseVector(0, Double.NegativeInfinity), DenseMatrix((-1.0, 1.0), (0.0, 1.0)), DenseVector(0, Double.PositiveInfinity))
    val ln3 = Parallelotope(DenseVector(Double.NegativeInfinity, 0), DenseMatrix((1.0, -1.0), (0.0, 1.0)), DenseVector(Double.PositiveInfinity, 0))
    expectResult(ln2) { ln3.linearAssignment(1, Array(1, 0), 0) }
    expectResult(ln2) { ln3.linearAssignment(1, Array(1), 0) }
    assert(empty.linearAssignment(1, Array(1, 0), 0).isEmpty)
  }

  test("non-deterministic assignment") {
    val nd1 = Parallelotope(DenseVector(Double.NegativeInfinity, -1), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity, 1))
    expectResult(nd1) { box.nonDeterministicAssignment(0) }
    expectResult(nd1) { nd1.nonDeterministicAssignment(0) }
    expectResult(nd1) { diamond.nonDeterministicAssignment(0) }
    val nd2 = Parallelotope(DenseVector(0, 0), DenseMatrix((2.0, 1.0), (2.0, -1.0)), DenseVector(1, 1))
    val nd3 = Parallelotope(DenseVector(Double.NegativeInfinity, -1), DenseMatrix((2.0, 1.0), (0.0, -2.0)), DenseVector(Double.PositiveInfinity, 1))
    expectResult(nd3) { nd2.nonDeterministicAssignment(0) }
    val nd4 = Parallelotope(DenseVector(Double.NegativeInfinity, 0), DenseMatrix((2.0, 1.0), (4.0, 0.0)), DenseVector(Double.PositiveInfinity, 2))
    expectResult(nd4) { nd2.nonDeterministicAssignment(1) }
    val nd5 = Parallelotope(DenseVector(10, -1), DenseMatrix((1.0, 0.0), (1.0, 1.0)), DenseVector(10, 1))
    val nd6 = Parallelotope(DenseVector(Double.NegativeInfinity, -11), DenseMatrix.eye(2), DenseVector(Double.PositiveInfinity, -9))
    expectResult(nd6) { nd5.nonDeterministicAssignment(0) }
    assert(empty.nonDeterministicAssignment(0).isEmpty)
  }

  test("linear inequalities") {
    val li1 = Parallelotope(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (1.0, -1.0)), DenseVector(0, 0))
    expectResult(li1) { diamond.linearInequality(Array(2, 0), 1) }
    expectResult(li1) { diamond.linearInequality(Array(2), 1) }
    assert(empty.linearInequality(Array(1, 0), -1).isEmpty)
  }

  test("linear disequalities") {
    val li1 = Parallelotope(DenseVector(-1, 0), DenseMatrix((1.0, 1.0), (1.0, -2.0)), DenseVector(0, 0))
    expectResult (li1) { li1.linearDisequality(Array(0,0), 1)}
    expectResult (empty) { li1.linearDisequality(Array(0,0), 0)}
    expectResult (li1) { li1.linearDisequality(Array(0,1), 1)}
    expectResult (li1) { li1.linearDisequality(Array(1,-2), 0.5)}
    expectResult (empty) { li1.linearDisequality(Array(1,-2), 0)}
  }

  test("union") {
    val u1 = Parallelotope(DenseVector(2, 0), DenseMatrix.eye(2), DenseVector(4, 2))
    val u2 = Parallelotope(DenseVector(-4, -1), DenseMatrix((-1.0, 3.0), (0.0, 1.0)), DenseVector(4, 2))
    expectResult(u2) { box union u1 }
    val u3 = Parallelotope(DenseVector(-1, -1), DenseMatrix((0.0, 1.0), (1.0, -1.0)), DenseVector(2, 4))
    expectResult(u3) { u1 union diamond }
    val u4 = Parallelotope(DenseVector(-4, 0), DenseMatrix.eye(2), DenseVector(-2, 2))
    val u5 = Parallelotope(DenseVector(-4, 0), DenseMatrix.eye(2), DenseVector(4, 2))
    expectResult(u5) { u4 union u1 }
    val u6 = Parallelotope(DenseVector(1, Double.NegativeInfinity), DenseMatrix((1.0, 0.0), (1.0, -1.0)), DenseVector(1, 1))
    val u7 = Parallelotope(DenseVector(0, Double.NegativeInfinity), DenseMatrix((1.0, 0.0), (0.0, -1.0)), DenseVector(0, 0))
    val u8 = Parallelotope(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(1, Double.PositiveInfinity))
    expectResult(u8) { u6 union u7 }
    val u9 = Parallelotope(DenseVector(0, 0), DenseMatrix.eye(2), DenseVector(0, Double.PositiveInfinity))
    expectResult(u8) { u9 union u8 }
    expectResult(u8) { u8 union u9 }
    val u10 = Parallelotope(DenseVector(2, 0), DenseMatrix.eye(2), DenseVector(2, 0))
    val u11 = Parallelotope(DenseVector(0, 2), DenseMatrix((0.0, 1.0), (1.0, -2.0)), DenseVector(1, 6))
    expectResult(u11) { u10 union u11 }
  }

  test("string representation") {
    expectResult(Seq("-1.0 <= x+y <= 1.0", "-1.0 <= x-y <= 1.0")) { diamond.mkString(IndexedSeq("x", "y")) }
    expectResult("[ empty ]") { empty.toString }
    expectResult("[ -Infinity <= v0 <= Infinity , -Infinity <= v1 <= Infinity ]") { full.toString }
  }
*/
}
