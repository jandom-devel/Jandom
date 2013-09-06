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

import it.unich.sci.jandom.domains.numerical.NumericalTopDomain
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

      val zeroCoeffs = Array.fill(dim)(0.0)
      val oneCoeffs = Array.fill(dim)(0.0)
      if (dim != 0) oneCoeffs(0) = 1

      describe(s"has an unique element which") {
        it("should be full") { assert(x.isTop) }
        it("should be bottom") { assert(x.isBottom) }
        it("should not be empty") { assert(!x.isEmpty) }
        it(s"should have dimension $dim") { expectResult(dim) (x.dimension)}
      }

      describe(s"has several operations on elements which") {
        it("should always return the same result as the input") {
          expectResult(x)(x widening x)
          expectResult(x)(x narrowing x)
          expectResult(x)(x union x)
          expectResult(x)(x intersection x)
          if (dim != 0) expectResult(x)(x.nonDeterministicAssignment(0))
          if (dim != 0) expectResult(x)(x.linearAssignment(0, Array.fill(dim)(0), 1))
          expectResult(x)(x.linearInequality(Array.fill(dim)(0), 0))
          expectResult(x)(x.linearDisequality(Array.fill(dim)(0), 0))
        }
      }

      describe(s"has a minimization operator which") {
        if (dim != 0) it("should return -Inf for not null linear forms") {
          expectResult(Double.NegativeInfinity)(x.minimize(oneCoeffs, 3))
        }
        it("should return the known coefficient for null linear forms") {
          expectResult(3)(x.minimize(zeroCoeffs, 3))
        }
      }

      describe(s"has a maximization operator which") {
        if (dim != 0) it("should return +Inf for not null linear forms") {
          expectResult(Double.PositiveInfinity)(x.maximize(oneCoeffs, 3))
        }
        it("should return the known coefficient for null linear forms") {
          expectResult(3)(x.maximize(zeroCoeffs, 3))
        }
      }

      describe(s"has a frequency operator which") {
        if (dim != 0) it("should return None for not null linear forms") {
          expectResult(None)(x.frequency(oneCoeffs, 3))
        }
        it("should return the known coefficient for null linear forms") {
          expectResult(Some(3))(x.frequency(zeroCoeffs, 3))
        }
      }

      describe(s"has dimension changing operators which") {
        if (dim > 0) they("reduce size of an element") {
            if (dim >= 2) expectResult(NumericalTopDomain.top(dim - 1))(x.delVariable(1))
            expectResult(NumericalTopDomain.top(dim - 1))(x.delVariable())
        }
        they("increase size of an element") {
          expectResult(NumericalTopDomain.top(dim + 1))(x.addVariable)
          expectResult(NumericalTopDomain.top(dim + 2))(x.addVariables(2))
        }
      }
    }
  }
}

