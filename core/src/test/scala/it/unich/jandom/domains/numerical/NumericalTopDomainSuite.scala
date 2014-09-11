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

package it.unich.jandom.domains.numerical

import org.scalatest.FunSpec

/**
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class NumericalTopDomainSuite extends FunSpec {

  val dims = Seq(0, 1, 2, 5)

  for (dim <- dims) {

    describe(s"The top numerical domain for dimension space $dim") {

      val x = NumericalTopDomain.top(dim)

      describe(s"has an unique element which") {
        it("should be full") { assert(x.isTop) }
        it("should be bottom") { assert(x.isBottom) }
        it("should not be empty") { assert(!x.isEmpty) }
        it(s"should have dimension $dim") { assertResult(dim) (x.dimension)}
      }

      describe(s"has several operations on elements which") {
        it("should always return the same result as the input") {
          assertResult(x)(x widening x)
          assertResult(x)(x narrowing x)
          assertResult(x)(x union x)
          assertResult(x)(x intersection x)
          if (dim != 0) assertResult(x)(x.nonDeterministicAssignment(0))
          if (dim != 0) assertResult(x)(x.linearAssignment(0, 1.0))
          assertResult(x)(x.linearInequality(0.0))
          assertResult(x)(x.linearDisequality(0.0))
        }
      }

      describe(s"has a minimization operator which") {
        if (dim != 0) it("should return -Inf for not constant linear forms") {
          assertResult(Double.NegativeInfinity)(x.minimize(LinearForm(3, 0 -> 1.0)))
        }
        it("should return the known coefficient for constant linear forms") {
          assertResult(3)(x.minimize(3.0))
        }
      }

      describe(s"has a maximization operator which") {
        if (dim != 0) it("should return +Inf for not constants linear forms") {
          assertResult(Double.PositiveInfinity)(x.maximize(LinearForm(3, 0 -> 1.0)))
        }
        it("should return the known coefficient for null linear forms") {
          assertResult(3)(x.maximize(3.0))
        }
      }

      describe(s"has a frequency operator which") {
        if (dim != 0) it("should return None for not null linear forms") {
          assertResult(None)(x.frequency(LinearForm(3, 0 -> 1.0)))
        }
        it("should return the known coefficient for null linear forms") {
          assertResult(Some(3))(x.frequency(3.0))
        }
      }

      describe(s"has dimension changing operators which") {
        if (dim > 0) they("reduce size of an element") {
            if (dim >= 2) assertResult(NumericalTopDomain.top(dim - 1))(x.delVariable(1))
            assertResult(NumericalTopDomain.top(dim - 1))(x.delVariable())
        }
        they("increase size of an element") {
          assertResult(NumericalTopDomain.top(dim + 1))(x.addVariable)
          assertResult(NumericalTopDomain.top(dim + 2))(x.addVariables(2))
        }
      }
    }
  }
}

