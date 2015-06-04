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
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.fixpoint

import org.scalatest.prop.PropertyChecks
import org.scalatest.FunSpec
import org.scalacheck.Gen

class UFactorySuite extends FunSpec with PropertyChecks {
  describe("A constant factory") {
    it("returns the same costant") {
      forAll { (c: Int) =>
        val f: UFactory[Int, Int] = c
        forAll { (x: Int) =>
          assertResult(c)(f(x))
        }
      }
    }
  }

  describe("A function factory") {
    val fun = { (x: Int) => 2 * x }
    val f: UFactory[Int, Int] = fun
    it("returns the same value as the function") {
      forAll { (x: Int) =>
        assertResult(fun(x))(f(x))
      }
    }
  }

  describe("A choose factory") {
    val f1 = { (x: Int) => x / 2 }
    val f2 = { (x: Int) => (x - 1) / 2 }
    val p = { (x: Int) => x % 2 == 0 }
    val f = UFactory.choose(p, f1, f2)
    it("returns the value of the two factories according to a predicate") {
      forAll { (x: Int) =>
        assertResult(if (p(x)) f1(x) else f2(x))(f(x))
      }
    }
  }
  
  describe("A memoize factory") {
    it("delegates to another factory but memoizes the result") {
      val progressiveFactory = new UFactory[Int, Int] {
        var current = 0
        def apply(x: Int)= {
          current += 1
          current
        }
      }      
      val f = UFactory.memoize(progressiveFactory)
      val f00 = f(0)
      val f10 = f(1)
      val f01 = f(0)
      val f20 = f(2)
      val f11 = f(1)
      assert( f00 === f01 )
      assert( f10 === f11 )
      assert( f00 !== f10 )
      assert( f00 !== f20 )
      assert( f10 !== f20 )      
    }    
    
  }
  
  describe("A cascade factory") {
    it("builds box assignments with correct memoization") {
      val gen = Gen.choose(0, 10)
      forAll(gen) { (d: Int) =>
        forAll { (x: Int, y: Int) =>
          val f = UFactory.cascade[Int,Int](Box.left[Int], d, Box.right[Int])
          val box1 = f(0)
          for (i <- 0 until d)
            assertResult(x)(box1(x, y))
          for (i <- 0 until 5)
            assertResult(y)(box1(x, y))
          val box2 = f(1)
          for (i <- 0 until d)
            assertResult(x)(box2(x, y))
          for (i <- 0 until 5)
            assertResult(y)(box2(x, y))
          val box3 = f(0)          
          for (i <- 0 until 5)
            assertResult(y)(box3(x, y))
        }
      }
    }
    
  }
}
