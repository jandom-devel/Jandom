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

package it.unich.jandom.domains.numerical

import org.scalatest.prop.TableFor1

import it.unich.jandom.domains.CartesianFiberedDomainSuite

/**
 * This is a common trait for test suites of numerical abstract domains. Some
 * of these tests check for properties which are not required for the correctness of an
 * abstract domain. Nonetheless, it is good to have them and, for the moment,
 * they remain here.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait NumericalDomainSuite extends CartesianFiberedDomainSuite {

  val dom: NumericalDomain

  lazy val someTypes = Table("type", ())

  lazy val someFibers = Table("fiber", (0 until 5) map { Seq.fill(_)(()) }: _*)

  lazy val someProperties = Table("property", dom.bottom(0), dom.bottom(1), dom.bottom(2), dom.bottom(3), dom.bottom(4), dom.bottom(4),
    dom.top(0), dom.top(1), dom.top(2), dom.top(3), dom.top(4), dom.top(5))
        
  /**
   * Some LinearForms type to use for the tests.
   */
  lazy val someLinearForms = Table[LinearForm[Double]]("linear form", LinearForm(1, 1, 1), LinearForm(0), LinearForm(2), LinearForm(0, -1, 0),
    LinearForm(2, 1, 1), LinearForm(2, 1, 0), LinearForm(-1, 0, 1, 1), LinearForm(-1, 0, 1, -1), LinearForm(0.25, 1, 0, 0),
    LinearForm(0.5, 1, 1, 0))  

  describe("Non deterministic assignment") {
    it("returns an upper bound") {
      forAll(someProperties) { (p) =>
        for (i <- 0 until p.dimension) {
          assert(p.nonDeterministicAssignment(i) >= p)
        }
      }
    }
    it("maps top to top") {
      forAll(someFibers) { (f) =>
        for (i <- 0 until f.length) {
          assert(dom.top(f.size).nonDeterministicAssignment(i).isTop)
        }
      }
    }
    it("maps empty to empty") {
      forAll(someFibers) { (f) =>
        val bottom = dom.bottom(f.size)
        if (bottom.isEmpty) {
          for (i <- 0 until f.length) {
            assert(bottom.nonDeterministicAssignment(i).isBottom)
          }
        }
      }
    }
  }

  describe("Linear assignment") {
    it("maps empty to empty") {
      forAll(someLinearForms) { (lf) =>
        forAll(someFibers) { (f) =>
          if (f.size >= lf.dimension) {
            val bottom = dom.bottom(f.size)
            if (bottom.isEmpty) {
              for (i <- 0 until f.size) {
                assert(bottom.linearAssignment(i, lf).isBottom)
              }
            }
          }
        }
      }
    }
    it("maps top to top if it is invertible") {
      forAll(someLinearForms) { (lf) =>
        forAll(someFibers) { (f) =>
          if (f.size >= lf.dimension) {
            for (i <- 0 until lf.dimension; if lf.homcoeffs(i) != 0) {
              assert(dom.top(f.size).linearAssignment(i, lf).isTop)
            }
          }
        }
      }
    }
    it("maps everything to bottom on unsatisfiable constraints") {
      forAll(someProperties) { (p) =>
        assert ( p.linearInequality(LinearForm(1)).isBottom )
      }
    }
  }

  describe("The identity linear assignment") {
    it("maps a property to itself") {
      forAll(someProperties) { (p) =>
        for (i <- 0 until p.dimension; lf = LinearForm.v[Double](i)) {
          assertResult(p)(p.linearAssignment(i, lf))
        }
      }
    }
  }

  describe("The linear inequality refinement") {
    it("returns a lower bound of the original property") {
      forAll(someProperties) { (p) =>
        forAll(someLinearForms) { (lf) =>
          if (p.dimension >= lf.dimension) assert(p.linearInequality(lf) <= p)
        }
      }
    }
  }

  describe("The linear disequality refinement") {
    it("returns a lower bound of the original property") {
      forAll(someProperties) { (p) =>
        forAll(someLinearForms) { (lf) =>
          if (p.dimension >= lf.dimension) assert(p.linearDisequality(lf) <= p)
        }
      }
    }
  }

  describe("The minimization/maximization/frequency operators") {
    it("returns -Inf/+Inf/None on a non constant linear form for top") {
      forAll(someFibers) { (f) =>
        forAll(someLinearForms) { (lf) =>
          if (f.size >= lf.dimension && lf.homcoeffs.exists { _ != 0 }) {
            assert(dom.top(f).minimize(lf).isNegInfinity)
            assert(dom.top(f).maximize(lf).isPosInfinity)
            assert(dom.top(f).frequency(lf).isEmpty)
          }
        }
      }
    }
    it("returns +Inf/-Inf/None on a non constant linear form for empty") {
      forAll(someFibers) { (f) =>
        forAll(someLinearForms) { (lf) =>
          if (f.size >= lf.dimension && lf.homcoeffs.exists { _ != 0 }) {
            val bot = dom.bottom(f)
            if (bot.isEmpty) {
              assert(dom.bottom(f).minimize(lf).isPosInfinity)
              assert(dom.bottom(f).maximize(lf).isNegInfinity)
              assert(dom.bottom(f).frequency(lf).isEmpty)
            }
          }
        }
      }
    }
    it("return constants for constant linear forms") {
      forAll(someProperties) { (p) =>
        forAll(someLinearForms) { (lf) =>
          if (p.dimension >= lf.dimension && lf.homcoeffs.forall { _ == 0 }) {
            assertResult(lf.known)(p.minimize(lf))
            assertResult(lf.known)(p.maximize(lf))
            assertResult(Some(lf.known))(p.frequency(lf))
          }
        }
      }
    }
  }
}
