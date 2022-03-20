/**
 * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.targets.parameters

import org.scalatest.funspec.AnyFunSpec

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.targets.parameters.NarrowingSpecs._

/**
 * A test for Narrowing hierarchy.
 * @author Gianluca Amato <gianluca.amato@unich.it>
 *
 */
class NarrowingsTest extends AnyFunSpec {
  val BoxDouble = BoxDoubleDomain()

  describe("DelayedNarrowing") {
    it("should delay for a fixed number of stepe") {
      val d1 = BoxDouble(Array(0), Array(3))
      val wd = new DelayedNarrowing(DefaultNarrowing, 2).get(BoxDouble)
      val d2 = BoxDouble(Array(1), Array(3))
      val d3 = wd(d1, d2)
      assertResult(BoxDouble(Array(1), Array(3))) { d3 }
      val d4 = BoxDouble(Array(2), Array(3))
      val d5 = wd(d3, d4)
      assertResult(BoxDouble(Array(2), Array(3))) { d5 }
      val d6 = BoxDouble(Array(3), Array(3))
      val d7 = wd(d5, d6)
      assertResult(BoxDouble(Array(2), Array(3))) { d7 }
    }

    it("should not delay when the delay parameter is set to zero") {
      val d1 = BoxDouble(Array(0), Array(3))
      val wd = new DelayedNarrowing(DefaultNarrowing, 0).get(BoxDouble)
      val d2 = BoxDouble(Array(1), Array(3))
      val d3 = wd(d1, d2)
      assertResult(BoxDouble(Array(0), Array(3))) { d3 }
    }
  }
}
