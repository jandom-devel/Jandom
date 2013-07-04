/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.sci.jandom.domains

import org.scalatest.FunSpec

/**
 * A functional specification for top domains.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class TopSuite extends FunSpec {
  val domains = Seq(TopDomain)

  for (dom <- domains) {
    describe("The top domain " + dom.getClass().getSimpleName()) {

      val x = TopDomain.top

      it("should have equal top and bottom elements") {
        assert(dom.top === dom.bottom)
       }

      describe("has an unique element which") {
        it("should be top") { assert(x.isTop) }
        it("should be bottom") { assert(x.isBottom) }
        it("should not be empty") { assert(!x.isEmpty) }
      }

      describe("has binary operations which") {
        it("should always return the same result as the input") {
          expectResult(x)(x widening x)
          expectResult(x)(x narrowing x)
          expectResult(x)(x union x)
          expectResult(x)(x intersection x)
        }
      }
    }
  }

}
