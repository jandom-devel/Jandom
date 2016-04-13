/**
 * Copyright 2016 Jandom Team
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
 * This is a trait for testing those domains where the intersection
 * operation computes the greatest lower bound.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait PreciseIntersectionSuite extends CartesianFiberedDomainSuite {
  describe("The intersection method") {
    it("returns a lower bound") {
      forAll(someCoupleProperties) { (p1, p2) =>
        assert((p1 intersection p2) <= p2)
        assert((p1 intersection p2) <= p1)
      }
    }
  }
}
