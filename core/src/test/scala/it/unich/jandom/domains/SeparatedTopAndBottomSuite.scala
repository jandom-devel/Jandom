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

/**
 * This is a trait for testing those domains where top != bottom in each fiber.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait SeparatedTopAndBottomSuite extends CartesianFiberedDomainSuite {
  describe("The top") {
    it("is not bottom") {
      forAll(someFibers) { (f) =>
        assert(!dom.top(f).isBottom)
        assert(dom.top(f) != dom.bottom(f))
        assert(dom.top(f) >= dom.bottom(f))
        assert(dom.top(f) > dom.bottom(f))        
        assert(dom.bottom(f) <= dom.top(f))
        assert(dom.bottom(f) < dom.top(f))
      }
    }    
  }
  describe("The bottom") {
    it("is not top") {
      forAll(someFibers) { (f) =>
        assert(dom.top(f) != dom.bottom(f))
        assert(dom.top(f) >= dom.bottom(f))
        assert(dom.top(f) > dom.bottom(f))        
        assert(dom.bottom(f) <= dom.top(f))
        assert(dom.bottom(f) < dom.top(f))
      }
    }    
  }
}
