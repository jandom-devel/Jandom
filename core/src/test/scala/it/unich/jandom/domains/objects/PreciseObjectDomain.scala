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

package it.unich.jandom.domains.objects

/**
 * A test trait for object domains with precise operators.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait PreciseObjectDomain extends ObjectDomainSuite {

  describe("The assignVariableToField method") {
    it("returns bottom when the destination variable is definitively null") {
      forAll(someAssignVarToField) { (prop, dst, field, src) =>
        whenever(prop.mustBeNull(dst)) {
          assert(prop.assignVariableToField(dst, field, src).isBottom)
        }
      }
    }
  }

  describe("The assignFieldToVariable method") {
    it("returns bottom when the source variable is definitively null") {
      forAll(someAssignFieldToVar) { (prop, dst, src, field) =>
        whenever(prop.mustBeNull(src)) {
          assert(prop.assignFieldToVariable(dst, src, field).isBottom)
        }
      }
    }
  }

  describe("The testNotNull method") {
    it("returns bottom if applied to a definite null variable") {
      forAll(somePropertiesAndVars) { (prop, v) =>
        if (prop.mustBeNull(v)) assert(prop.testNotNull(v).isBottom)
      }
    }
  }

  describe("The testNull method") {
    it("is identitity if applied to a definite null variable") {
      forAll(somePropertiesAndVars) { (prop, v) =>
        if (prop.mustBeNull(v)) assert(prop.testNull(v) === prop)
      }
    }
    it("is equivalent to assignNull for top") {
      forAll(someFibersAndVars) { (fiber, v) =>
        assert(dom.top(fiber).testNull(v) === dom.top(fiber).assignNull(v))
      }
    }
  }
}
