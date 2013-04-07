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

package it.unich.sci.jandom.utils.numberext

import org.scalatest.FunSuite

/**
 * Test suite for generic extended numbers
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class GenericNumberExtSuite extends FunSuite { 
   
  import GenericNumberExt._

  test("equality on infinities") {
    assert(PositiveInfinity[Int] == PositiveInfinity[Int], "+Inf = +Inf")
    assert(NegativeInfinity[Int] == NegativeInfinity[Int], "-Inf = -Inf")
    assert(PositiveInfinity[Int] != NegativeInfinity[Int], "+Inf != -Inf")
    assert(NegativeInfinity[Int] != PositiveInfinity[Int], "-Inf != +Inf")
    assert(NegativeInfinity[Int] != NaN[Int], "-Inf != NaN")
    assert(PositiveInfinity[Int] != NaN[Int], "+Inf != NaN")
    assert(NaN[Int] == NaN[Int], "NaN = NaN")
    assert(NaN[Int] != NegativeInfinity[Int], "NaN != +Inf")
    assert(NaN[Int] != PositiveInfinity[Int], "NaN != +Inf")    
  }	    


  test("equality on regular numbers") {
    val n1 = GenericNumberExt[Int](2)
    val n2 = GenericNumberExt[Int](2)
    val n3 = GenericNumberExt[Int](3)
    val n4 = GenericNumberExt[Float](2)
    assert(n1===n1)
    assert(n1===n2)
    assert(n1!=n3)
    assert(n1==n4)
    assert(n2==n1)
    assert(n2==n2)
    assert(n2!=n3)
    assert(n2==n4)
    assert(n3!=n1)
    assert(n3!=n2)
    assert(n3==n3)
    assert(n3!=n4)
    assert(n4==n1)
    assert(n4==n2)
    assert(n4!=n3)
    assert(n4==n4)
  }
  
  test("matches") {
    val x = PositiveInfinity    
    assert( x match {
      case PositiveInfinity => true
      case _ => false
    } )
  }
} 
