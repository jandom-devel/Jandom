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

package it.unich.jandom.domains.objects

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test suite for unordered pairs.
 * @author Gianluca Amato <gamato@unich.it>
 */
class UPSuite extends AnyFunSuite {
  test ("UP's are unrodered"){ assert( UP(2,3) == UP(3,2) )}
  test ("UP's may be trasnformed into ordered pairs") { assert( UP.unapply(UP(3,2)) === Some((2,3)) ) }
  test ("replacement of elements") {
    assert ( UP(2,3).replace(4,3) === UP(2,4))
    assert ( UP(2,3).replace(3,3) === UP(2,3))
    assert ( UP(2,3).replace(3,5) === UP(2,3))
  }
  test ("containment check") {
    assert ( UP(2,3) contains 3 )
    assert ( ! (UP(2,3) contains 4) )
  }
}
