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

import org.scalatest.prop.TableFor1

/**
 * This is a base trait for testing cartesian fibered abstract domains. Some of these
 * test are not required of any correct implementation of cartesian domains, but we leave
 * them here until some domain actually violates them.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait CartesianFiberedDomainSuite extends AbstractDomainSuite {

  val dom: CartesianFiberedDomain

  /**
   * Some fiber components to use for the tests.
   */
  val someTypes: TableFor1[dom.FiberComponent]

  /**
   * Some fibers to use for the tests.
   */
  val someFibers: TableFor1[dom.Fiber]

  /**
   * A table of fibers and related valid variables
   */
  lazy val someFibersAndVars = Table((someFibers.heading, "var"),
    (for (fiber <- someFibers; v <- 0 until fiber.size) yield (fiber, v)): _*)

  /**
   * A table of fibers and two valid variables
   */
  lazy val someFibersAndTwoVars = Table((someFibers.heading, "v1", "v2"),
    (for (fiber <- someFibers; v <- 0 until fiber.size; w <- 0 until fiber.size) yield (fiber, v, w)): _*)

  /**
   * A table of properties and related valid variables
   */
  lazy val somePropertiesAndVars = Table((someProperties.heading, "var"),
    (for (p <- someProperties; v <- 0 until p.dimension) yield (p, v)): _*)

  /**
   * A table of properties and two valid variables
   */
  lazy val somePropertiesAndTwoVars = Table((someProperties.heading, "v1", "v2"),
    (for (p <- someProperties; v <- 0 until p.dimension; w <- 0 until p.dimension) yield (p, v, w)): _*)

  /**
   * A table for a couple of compatible properties
   */
  lazy val someCoupleProperties = Table((someProperties.heading + " 1", someProperties.heading + " 2"),
    (for (p1 <- someProperties; p2 <- someProperties; if p1.fiber == p2.fiber) yield (p1, p2)): _*)

  /**
   * This may be used to check that `p` is a non-extremal property, i.e. it is not
   * top or bottom.
   */
  def nonExtremalProperty(p: dom.Property) {
    it("is not bottom") { assert(!p.isBottom) }
    it("is not top") { assert(!p.isTop) }
    it("is not empty") { assert(!p.isEmpty) }
    it("is bigger than bottom") { assert(p > p.bottom) }
    it("is smaller than top") { assert(p < p.top) }
  }

  describe("The bottom element for a given fiber")({
    it("is smaller than top") { forAll(someFibers) { (fiber) => dom.bottom(fiber) <= dom.top(fiber) } }
    it("is bottom") { forAll(someFibers) { (fiber) => assert(dom.bottom(fiber).isBottom) } }
    it("has dimension equal to the size of the fiber") { forAll(someFibers) { (fiber) => assert(dom.bottom(fiber).dimension === fiber.size) } }
    it("is smaller than all other elements") { (forAll(someProperties) { ((p) => assert(dom.bottom(p.fiber) <= p)) }) }
  })

  describe("The top element for a given fiber")({
    it("is bigger than bottom") { forAll(someFibers) { (fiber) => dom.top(fiber) >= dom.bottom(fiber) } }
    it("is top") { forAll(someFibers) { (fiber) => assert(dom.top(fiber).isTop) } }
    it("is not empty") { forAll(someFibers) { (fiber) => assert(!dom.top(fiber).isEmpty) } }
    it("has dimension equal to the size of the fiber") { forAll(someFibers) { (fiber) => assert(dom.top(fiber).dimension === fiber.size) } }
    it("is bigger than all other elements") { (forAll(someProperties) { ((p) => assert(p <= dom.top(p.fiber))) }) }
  })

  describe("The addVariable method")({
    it("transforms top to top") {
      forAll(someFibers) { (fiber) =>
        forAll(someTypes) { t =>
          assert(dom.top(fiber).addVariable(t).isTop)
        }
      }
    }
    it("adds a dimension") {
      (
        forAll(someProperties) {
          ((p) =>
            forAll(someTypes) {
              ((t) =>
                assert(p.addVariable(t).dimension === p.dimension + 1))
            })
        })
    }
  })

  describe("The delVariable method") {
    it("transforms top to top") {
      forAll(someFibersAndVars) { (fiber, v) =>
        assert(dom.top(fiber).delVariable(v).isTop)
      }
    }
    it("removes a dimension") {
      forAll(somePropertiesAndVars) { (p, v) =>
        assert(p.delVariable(v).dimension === p.dimension - 1)
      }
    }
  }

  describe("The mapVariables method") {
    val rhos = Table("rho", Seq(-1, -1, -1, 0), Seq(2, 1, -1, 0), Seq(0, 1, 2, 3), Seq(3, 0, 2, 1))
    it("transforms top to top") {
      forAll(someFibers) { (fiber) =>
        whenever(fiber.size == 4) {
          forAll(rhos) { (rho) =>
            val mapped = dom.top(fiber).mapVariables(rho)
            assert(mapped.isTop, s"for ${mapped}")
          }
        }
      }
    }
    it("removes dimensions mapped to -1") {
      forAll(someFibers) { (fiber) =>
        whenever(fiber.size == 4) {
          forAll(rhos) { (rho) =>
            assert(dom.top(fiber).mapVariables(rho).dimension == 4 - rho.count(_ == -1))
          }
        }
      }
    }
  }

  describe("The union method") {
    it("is idempotent") {
      forAll(someProperties) {
        (p) => assertResult(p)(p union p)
      }
    }
    it("returns an upper bound") {
      forAll(someCoupleProperties) { (p1, p2) =>
        assert((p1 union p2) >= p2)
        assert((p1 union p2) >= p1)
      }
    }
    it("is associative") {
      forAll(someProperties) { (p1) =>
        forAll(someProperties) { (p2) =>
          forAll(someProperties) { (p3) =>
            whenever(p1.fiber == p2.fiber && p2.fiber == p3.fiber) {
              assert(((p1 union p2) union p3) === (p1 union (p2 union p3)))
            }
          }
        }
      }
    }
    it("has bottom as neutral element") {
      forAll(someProperties) { (p) =>
        assert((p union p.bottom) === p)
        assert((p.bottom union p) === p)
      }
    }
    it("has top as absorbing element") {
      forAll(someProperties) { (p) =>
        assert((p union p.top) === p.top)
        assert((p.top union p) === p.top)
      }
    }
  }

  describe("The intersection method") {
    it("is idempotent") {
      forAll(someProperties) { (p) =>
        assert((p intersection p) === p)
      }
    }
    it("is associative") {
      forAll(someProperties) { (p1) =>
        forAll(someProperties) { (p2) =>
          forAll(someProperties) { (p3) =>
            whenever(p1.fiber == p2.fiber && p2.fiber == p3.fiber) {
              assert(((p1 intersection p2) intersection p3) === (p1 intersection (p2 intersection p3)))
            }
          }
        }
      }
    }
    it("has top as neutral element") {
      forAll(someProperties) { (p) =>
        assert((p intersection p.top) === p)
        assert((p.top intersection p) === p)
      }
    }
    it("has bottom as absorbing element") {
      forAll(someProperties) { (p) =>
        assert((p intersection p.bottom) === p.bottom)
        assert((p.bottom intersection p) === p.bottom)
      }
    }
  }
}
