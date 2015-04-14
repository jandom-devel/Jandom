
package it.unich.jandom.domains.objects

import org.scalatest.FunSpec
import org.scalatest.prop.TableFor1

/**
 * This suite implements tests for those object domain which precisely track
 * definite nullness.
 * Created by amato on 4/1/14.
 */
trait PreciseDefiniteNullness extends ObjectDomainSuite {

  describe("The bottom element") {
    it("has all variable definitively null") {
      forAll (someFibersAndVars) { (fiber, i) => 
        assert(dom.bottom(fiber).mustBeNull(i))
      }
    }
  }
  
  describe("The assignNull method") {
    it("makes variable definitively null") {
      forAll (somePropertiesAndVars) { (p,i) =>
        assert(p.assignNull(i).mustBeNull(i))
      }
    }
    it("should produce a lesser property than top") {
      forAll (someFibersAndVars) { (f,i) =>           
        val top = dom.top(f)
        assert(top.assignNull(i) < top)
      }
    }
  }

  describe("The testNull method") {
    it("is the identity on bottom") {
      forAll(someFibersAndVars) { (fiber, j) =>
        assert(dom.bottom(fiber).testNull(j).isBottom)
      }
    }
  }

  describe("The testNotNull method") {
    it("is identity on bottom") {
      forAll(someFibersAndVars) { (fiber, j) =>
        assert(dom.bottom(fiber).testNotNull(j).isBottom)
      }
    }
  }
}
