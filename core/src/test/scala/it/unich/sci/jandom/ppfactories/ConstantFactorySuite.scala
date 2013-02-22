/**
 * Copyright 2013 Gianluca Amato
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

package it.unich.sci.jandom
package ppfactories

import targets.MockTarget
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Test suite for constant "per program point" factories.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class ConstantFactorySuite extends FunSpec  {
  describe("Constant Factory") {
    it ("should return the same object each time it is called")  {
      val dwf: PPFactory[MockTarget,String] = "prova"
      val d0 = dwf(0)
      val d1= dwf(0)
      val d2= dwf(2)
      expectResult(d0) { d1 }
      expectResult(d1) { d2 }
    }
  }
}