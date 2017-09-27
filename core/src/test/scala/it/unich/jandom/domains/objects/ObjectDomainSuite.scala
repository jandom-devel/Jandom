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

import org.scalatest.FunSpec
import org.scalatest.prop.TableDrivenPropertyChecks

import it.unich.jandom.domains.CartesianFiberedDomainSuite
import it.unich.jandom.objectmodels.ObjectModel

/**
 * This is a common trait for test suites of object domains. It contains all the standard tests
 * valid for all object domains.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait ObjectDomainSuite extends CartesianFiberedDomainSuite with TableDrivenPropertyChecks {
  this: FunSpec =>

  val om: ObjectModel
  val dom: ObjectDomain[om.type]

  val someAssignVariable = Table((someProperties.heading, "dst", "src"),
    (for (p <- someProperties; dst <- 0 until p.dimension; src <- 0 until p.dimension; if om.lteq(p.fiber(src), p.fiber(dst))) yield (p, dst, src)): _*)

  val someAssignFieldToVar = Table((someProperties.heading, "dst", "src", "f"),
    (for (p <- someProperties; dst <- 0 until p.dimension; src <- 0 until p.dimension; f <- om.fields(p.fiber(src)); if om.lteq(om.typeOf(f), p.fiber(dst))) yield (p, dst, src, f)): _*)

  val someAssignVarToField = Table((someProperties.heading, "dst", "f", "src"),
    (for (p <- someProperties; dst <- 0 until p.dimension; f <- om.fields(p.fiber(dst)); src <- 0 until p.dimension; if om.lteq(p.fiber(src), om.typeOf(f))) yield (p, dst, f, src)): _*)

  val someCast = Table((someProperties.heading, "v", someTypes.heading),
    (for (p <- someProperties; v <- 0 until p.dimension; t <- someTypes; if om.lteq(t, p.fiber(v))) yield (p, v, t)): _*)

  describe("The top element") {
    it("has all variables possibly and not definitively null") {
      forAll(someFibersAndVars) { (fiber, i) =>
        val top = dom.top(fiber)
        assert(top.mayBeNull(i))
        assert(!top.mustBeNull(i))
        for (j <- om.fields(fiber(i))) {
          assert(!top.mustBeNull(i), Seq(j))
          for (k <- om.fields(om.typeOf(j))) assert(!top.mustBeNull(i), Seq(j, k))
        }
      }
    }
    it("has all variable pairs possibly and not definitively aliased and sharing") {
      forAll(someFibersAndTwoVars) { (fiber, i, j) =>
        whenever(i != j) {
          val d = dom.top(fiber)
          assert(d.mayBeWeakAliases(i, j))
          assert(!d.mustBeWeakAliases(i, j))
          assert(d.mayShare(i, j))
          assert(!d.mustShare(i, j))
        }
      }
    }
  }

  describe("The mayBeNull method") {
    it("is true when the variables must be null") {
      forAll(somePropertiesAndVars) { (p, v) =>
        whenever(p.mustBeNull(v)) { assert(p.mayBeNull(v)) }
      }
    }
  }

  describe("The mayBeWeakAliases method") {
    it("is true when applied to the same variable") {
      forAll(somePropertiesAndVars) { (p, v) =>
        assert(p.mustBeWeakAliases(v, v))
      }
    }
    it("is true when two variables must be weak aliases") {
      forAll(somePropertiesAndTwoVars) { (p, v1, v2) =>
        whenever(p.mustBeWeakAliases(v1, v2)) { assert(p.mayBeWeakAliases(v1, v2)) }
      }
    }
    it("is true when two variables must be both null") {
      forAll(somePropertiesAndTwoVars) { (p, v1, v2) =>
        whenever(p.mustBeNull(v1) && p.mustBeNull(v2)) { assert(p.mayBeWeakAliases(v1, v2)) }
      }
    }
    it("is true when two variables may be aliases") {
      forAll(somePropertiesAndTwoVars) { (p, v1, v2) =>
        whenever(p.mayBeAliases(v1, v2)) { assert(p.mayBeWeakAliases(v1, v2)) }
      }
    }
  }

  describe("The mayShare method") {
    it("is true when two variables may be aliased") {
      forAll(somePropertiesAndTwoVars) { (p, v1, v2) =>
        whenever(p.mayBeAliases(v1, v2)) { assert(p.mayShare(v1, v2)) }
      }
    }
    it("is true when two variables must share") {
      forAll(somePropertiesAndTwoVars) { (p, v1, v2) =>
        whenever(p.mustShare(v1, v2)) { assert(p.mayShare(v1, v2)) }
      }
    }
    it("is false if a variable is definitively null") {
      forAll(somePropertiesAndTwoVars) { (p, v1, v2) =>
        whenever(p.mustBeNull(v1) || p.mustBeNull(v2)) { !p.mayShare(v1, v2) }
      }
    }
  }

  describe("The assignNull method") {
    it("makes variables possibly null") {
      forAll(somePropertiesAndVars) { (p, v) =>
        assert(p.assignNull(v).mayBeNull(v))
      }
    }
  }

  describe("The assignVariable method") {
    it("makes variables possibly weak aliases") {
      forAll(someAssignVariable) { (p, dst, src) =>
        assert(p.assignVariable(dst, src).mayBeWeakAliases(dst, src))
      }
    }
    it("propagates possibly sharing") {
      forAll(someAssignVariable) { (p, dst, src) =>
        forAll(Table("vother", (0 until p.dimension): _*)) { (vother) =>
          whenever(p.mayShare(src, vother)) { p.assignVariable(dst, src).mayShare(dst, src) }
        }
      }
    }
  }

  describe("The assignFieldToVariable method") {
    it("makes variables possibly share if source and target are not null") {
      forAll(someAssignFieldToVar) { (p, i, j, f) =>
        whenever(!p.mustBeNull(i) && !p.mustBeNull(j, Seq(f))) {
          assert(p.assignFieldToVariable(i, j, f).mayShare(i, j))
        }
      }
    }
    it("propagate possibly nullness") {
      forAll(someAssignFieldToVar) { (p, i, j, f) =>
        whenever(p.mayBeNull(j, Seq(f))) {
          assert(p.assignFieldToVariable(i, j, f).mayBeNull(i))
        }
      }
    }
    it("propagate possibly non nullness if target is not null") {
      forAll(someAssignFieldToVar) { (p, i, j, f) =>
        whenever(!p.mustBeNull(i) && !p.mustBeNull(j, Seq(f))) {
          assert(!p.assignFieldToVariable(i, j, f).mustBeNull(i))
        }
      }
    }
  }

  describe("The assignVariableToField method") {
    it("makes variables possibly share if source and target variable are not null") {
      forAll(someAssignVarToField) { (p, i, f, j) =>
        whenever(!p.mustBeNull(i) && !p.mustBeNull(j)) {
          assert(p.assignVariableToField(i, f, j).mayShare(i, j))
        }
      }
    }
    it("propagate possibly nullness") {
      forAll(someAssignVarToField) { (p, i, f, j) =>
        whenever(p.mayBeNull(j)) {
          assert(p.assignVariableToField(i, f, j).mayBeNull(i, Seq(f)))
        }
      }
    }
    it("propagate possibly non nullness if target variable is not null") {
      forAll(someAssignVarToField) { (p, i, f, j) =>
        whenever(!p.mustBeNull(i) && !p.mustBeNull(j)) {
          assert(!p.assignVariableToField(i, f, j).mustBeNull(i, Seq(f)))
        }
      }
    }
  }

  describe("The cast method") {
    it("propagate possibly nullness") {
      forAll(someCast) { (p, i, t) =>
        whenever(p.mayBeNull(i)) {
          assert(p.castVariable(i, t).mayBeNull(i))
        }
      }
    }
    it("propagate possibly non-nullness if no cast error") {
      forAll(someCast) { (p, i, t) =>
        whenever(!p.mustBeNull(i)) {
          val res = p.castVariable(i, t)
          assert(res.isBottom || (! res.mustBeNull(i)))
        }
      }
    }
  }

  describe("The addFreshVariable method") {
    it("add a variable") {
      forAll(someProperties) { (p) =>
        forAll(someTypes) { (t) =>
          assert(p.addFreshVariable(t).dimension === p.dimension + 1)
        }
      }
    }

    it("adds a variable which cannnot share and alias with other variables and cannot be null") {
      forAll(someProperties) { (p) =>
        forAll(someTypes) { (t) =>
          val p2 = p.addFreshVariable(t)
          assert(!p2.mustBeNull(p.dimension))
          assert(p2.dimension === p.dimension + 1)
          for (j <- 0 until p.dimension) {
            assert(!p2.mustShare(p.dimension, j))
            assert(!p2.mustBeAliases(p.dimension, j))
          }
        }
      }
    }
  }

  describe("The addUnknownVariable method") {
    forAll(someProperties) { (p) =>
      forAll(someTypes) { (t) =>
        assert(p.addUnknownVariable(t).dimension === p.dimension + 1)
      }
    }
  }
}
