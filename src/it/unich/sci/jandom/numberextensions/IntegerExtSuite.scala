/**
 *
 * Copyright 2011 Gianluca Amato
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

package it.unich.sci.jandom.numberextensions

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers 
import org.scalacheck.Prop._

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class IntegerExtSuite extends FunSuite with Checkers {
  import IntegerExt._

  test("equality on infinities") {
    assert(PositiveInfinity == PositiveInfinity, "+Inf = +Inf")
    assert(NegativeInfinity == NegativeInfinity, "-Inf = -Inf")
    assert(PositiveInfinity != NegativeInfinity, "+Inf != -Inf")
    assert(NegativeInfinity != PositiveInfinity, "-Inf != +Inf")
    assert(NegativeInfinity != NaN, "-Inf != NaN")
    assert(PositiveInfinity!= NaN, "+Inf != NaN")
    assert(NaN== NaN, "NaN = NaN")
    assert(NaN != NegativeInfinity, "NaN != +Inf")
    assert(NaN != PositiveInfinity, "NaN != +Inf")    
    assert(NaN != IntegerExt(2), "NaN != 2")
  }	    


  test("equality on regular numbers") {
    val n1 = IntegerExt(2)
    val n2 = IntegerExt(2)
    val n3 = IntegerExt(3)
    assert(n1===n1)
    assert(n1===n2)
    assert(n1!=n3)
    assert(n2==n1)
    assert(n2==n2)
    assert(n2!=n3)
    assert(n3!=n1)
    assert(n3!=n2)
    assert(n3==n3)
  }
  
  test("operations on regular numbers") {
    check( (w:Int, h:Int) => IntegerExt(w) + IntegerExt(h) == IntegerExt(w+h))
    check( (w:Int, h:Int) => IntegerExt(w) - IntegerExt(h) == IntegerExt(w-h))
    check( (w:Int) => +(IntegerExt(w)) == IntegerExt(w) )
    check( (w:Int) => -(IntegerExt(w)) == IntegerExt(-w) )      
  } 
  
  test("sum on infinities") {
    expect (PositiveInfinity) { IntegerExt(3) + PositiveInfinity }
    expect (PositiveInfinity) { PositiveInfinity + IntegerExt(3)  }
    expect (PositiveInfinity) { PositiveInfinity + PositiveInfinity  }
    expect (NegativeInfinity) { IntegerExt(3) + NegativeInfinity }
    expect (NegativeInfinity) { NegativeInfinity + IntegerExt(3)  }
    expect (NegativeInfinity) { NegativeInfinity + NegativeInfinity }
    expect (NaN) { NaN + IntegerExt(3) }
    expect (NaN) { IntegerExt(3) + NaN }
    expect (NaN) { PositiveInfinity + NegativeInfinity }
    expect (NaN) { NegativeInfinity + PositiveInfinity }
    expect (NaN) { NaN + NaN }
  }
  
  test("difference on infinities") {
    expect (NegativeInfinity) { IntegerExt(3) - PositiveInfinity }
    expect (PositiveInfinity) { PositiveInfinity - IntegerExt(3)  }
    expect (NaN) { PositiveInfinity - PositiveInfinity  }
    expect (PositiveInfinity) { IntegerExt(3) - NegativeInfinity }
    expect (NegativeInfinity) { NegativeInfinity - IntegerExt(3)  }
    expect (NaN) { NegativeInfinity - NegativeInfinity }
    expect (NaN) { NaN - IntegerExt(3) }
    expect (NaN) { IntegerExt(3) - NaN }
    expect (PositiveInfinity) { PositiveInfinity - NegativeInfinity }
    expect (NegativeInfinity) { NegativeInfinity - PositiveInfinity }
    expect (NaN) { NaN - NaN }
  }   
    
  test ("toString method") {
    check( (w:Int) => IntegerExt(w).toString == w.toString )
    expect ("+Inf") { PositiveInfinity.toString }
    expect ("-Inf") { NegativeInfinity.toString }
    expect ("NaN") { NaN.toString }
  }
}