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
 * This test is for domains which precisely implements possible
 * pair sharing (or, in turn, definite non pair-sharing).
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait PrecisePossiblePairSharing extends ObjectDomainSuite {
  describe("The bottom element") {
    it("has all variables definitively not sharing") {
      forAll(someFibersAndTwoVars) { (fiber, i, j) =>
        assert(!dom.bottom(fiber).mayShare(i, j))
      }
    }
  }
  
  describe("The assignNull method") {
    it("makes variable definitively not sharing") {
      forAll(somePropertiesAndTwoVars) { (p,i,j) =>
        assert (! p.assignNull(i).mayShare(i,j))
      }
    }
  }
  
  describe("The addFreshVariable method") {
    it("creates a variable definitively not sharing") {
      forAll(somePropertiesAndVars) { (p,i) =>
        forAll(someTypes) { (t) => 
          val newp = p.addFreshVariable(t)
          assert(! p.addFreshVariable(t).mayShare(p.dimension, i), s"Variable ${i} share with ${p.dimension} in ${newp}")
        }
      }  
    }
  }
}
