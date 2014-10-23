/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
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

import org.scalatest.FunSpec
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor2}

/**
 * This is a common trait for test suites of abstract domains.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait AbstractDomainSuite extends FunSpec with TableDrivenPropertyChecks {
 
  /**
   * The abstract domain to test
   */
  val dom: AbstractDomain

  /**
   * A table of some properties upon which tests should be applied
   */
  val someProperties: TableFor1[dom.Property]

  /**
   * A table of pairs of compatible properties upon which tests should be applied. Properties
   * are compatibles when it is possible to apply comparison and binary operations on them.
   */
  val someCoupleProperties: TableFor2[dom.Property, dom.Property]

  describe("The approximated equality between properties") {
    it("is reflexive") { forAll(someProperties) { (p) => assert(p == p) } }
    it("is symmetric") { forAll(someCoupleProperties) { (p1, p2) => assert((p1 == p2) == (p2 == p1)) } }
    it("is transitive") {
      forAll(someCoupleProperties) { (p1, p2) =>
        forAll(someCoupleProperties) { (p3, p4) =>
          {
            if (p1 == p2 && (p2 eq p3) && p3 == p4) assert(p1 == p4)

          }
        }
      }
    }
  }
  describe("The approximated non-strict ordering on properties") {
    it("is reflexive") { forAll(someProperties) { (p) => assert(p <= p) } }
    it("is anti-symmetric") { forAll(someCoupleProperties) { (p1, p2) => if (p1 <= p2 && p1 >= p2) assert(p1 == p2) } }
    it("is transitive") {
      forAll(someCoupleProperties) { (p1, p2) =>
        forAll(someCoupleProperties) { (p3, p4) =>
          {
            if (p1 <= p2 && (p2 eq p3) && p3 <= p4) assert(p1 <= p4)
            if (p1 >= p2 && (p2 eq p3) && p3 >= p4) assert(p1 >= p4)
          }
        }
      }
    }
    it("is implied by equality") {
      forAll(someCoupleProperties) {
        case (p1, p2) =>
          if (p1 == p2) { assert(p1 <= p2); assert(p1 >= p2) }
      }
    }
  }

  describe("The approximated strict ordering on properties") {
    it("is negated by the opposite approximated non-strict inequality") {
      forAll(someCoupleProperties) {
        case (p1, p2) =>
          if (p1 <= p2) assert(!(p1 > p2))
          if (p1 >= p2) assert(!(p1 < p2))
      }
    }
    it("implies the corresponding approximated non-strict inequality and disequality") {
      forAll(someCoupleProperties) {
        case (p1, p2) =>
          if (p1 < p2) { assert(p1 <= p2); assert(p1 != p2) }
          if (p1 > p2) { assert(p1 >= p2); assert(p1 != p2) }
      }
    }
  }

  describe("The union method") {
    it("it returns an abstract object which is an upper bound of the parameters") {
      forAll(someCoupleProperties) { (p1, p2) =>
        // the check is convoluted since we only have an approximation of the abstract ordering
        assert(!((p1 union p2) < p2))
        assert(!((p1 union p2) < p1))
      }
    }
  }

  describe("The widening method") {
    it("it returns an abstract object which is a possible upper bound of the parameters") {
      forAll(someCoupleProperties) { (p1, p2) =>
        // the check is convoluted since we only have an approximation of the abstract ordering
        assert(!((p1 widening p2) < p2))
        assert(!((p1 widening p2) < p1))
      }
    }
  }

  describe("The narrowing method") {
    it("it returns an abstract object which is a possible lower bound of the first parameter") {
      forAll(someCoupleProperties) { (p1, p2) =>
        if (p2 <= p1) {
          // the check is convoluted since we only have an approximation of the abstract ordering          
          assert(!((p1 narrowing p2) > p1))
          assert(!((p1 narrowing p2) < p2))
        }
      }
    }
  }
}
