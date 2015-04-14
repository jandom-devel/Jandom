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

import it.unich.jandom.domains.CartesianFiberedDomainSuite
import org.scalatest.FunSpec

/**
 * This is a trait for test for cartesian domains with precise operations
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait PreciseFiberChange  {
  this: FunSpec with CartesianFiberedDomainSuite =>

  describe("The delVariable method") {
    it("transforms bottom top bottom") {
      for (fiber <- someFibers; v <- 0 until fiber.size) {
        assert(dom.bottom(fiber).delVariable(v).isBottom)
      }
    }
    it("undoes the effect of an addVariable") {
    }
  }

  describe("The mapVariables method") {
    val rhos = Seq(Seq(-1, -1, -1, 0), Seq(2, 1, -1, 0), Seq(0, 1, 2, 3), Seq(3, 0, 2, 1))
    it("transforms bottom to bottom") {
      for (fiber <- someFibers; rho <- rhos; if fiber.size >= rho.size)
        assert(dom.bottom(fiber).mapVariables(rho).isBottom)
    }
  }

}
