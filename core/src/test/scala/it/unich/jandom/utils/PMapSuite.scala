/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.utils

import org.scalatest.FunSpec
import it.unich.jandom.utils.PMaps._

class PMapSuite extends FunSpec {
  val pa = Parameter[Int]
  val valuepa = 2

  val pb = Parameter[Int]
  val valuepb = 1

  val pc = Parameter[String]
  val valuepc = "foo"

  val pz = Parameter[String]("default")
  val valuepz = "nodefault"

  val m1 = (pa --> valuepa) +: (pc --> valuepc) +: PMap.empty
  val m2 = (pb --> valuepb) +: m1
  val m3 = (pz --> valuepz) +: m1
  
  describe("A PMap") {
    it("allows access to parameter of the right type") {
      assertResult(valuepa)(m1(pa))
      assertResult(valuepa)(m2(pa))
      assertResult(valuepc)(m1(pc))
      assertResult(valuepc)(m2(pc))
      assertResult(valuepb)(m2(pb))
      assertResult(pz.default)(m1(pz))
      assertResult(pz.default)(m2(pz))
      assertResult(valuepz)(m3(pz))
    }

    it("allows optional access to all parameters") {
      assertResult(Some(valuepa))(m1.get(pa))
      assertResult(Some(valuepa))(m2.get(pa))
      assertResult(Some(valuepc))(m1.get(pc))
      assertResult(Some(valuepc))(m2.get(pc))
      assertResult(None)(m1.get(pb))
      assertResult(Some(valuepb))(m2.get(pb))
      assertResult(Some(pz.default))(m1.get(pz))
      assertResult(Some(pz.default))(m2.get(pz))
      assertResult(Some(valuepz))(m3.get(pz))
    }

    it("is automatically converted to a parameter map with less parameters") {
      val mconv: pa.type +: PNil = m1
      assertResult(valuepa)(mconv(pa))
      assertResult(None)(mconv.get(pb))
      assertResult(Some(valuepc))(mconv.get(pc))
      
      val mconv2: pb.type +: pa.type +: PNil = m2
      assertResult(valuepa)(mconv2(pa))
      assertResult(valuepb)(mconv2(pb))
      assertResult(Some(valuepc))(mconv2.get(pc))
    }
    
    it("may be viewed as a parameter map with less parameters") {
      def f[M <% pa.type +: PNil](m: M) = m(pa)        
      assertResult(valuepa)(f(m1))
      assertResult(valuepa)(f(m2))      
    }

    it("may be checked for the presence of a parameter") {
      intercept[MissingParameterException](m1.check(pb))
      assertResult(m1)(m1.check(pa))
    }
  }
}
