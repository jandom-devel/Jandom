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

package it.unich.jandom.widenings

import org.scalatest.FunSuite
import it.unich.jandom.domains.numerical.BoxDoubleDomain

/**
 * A test for the default widening.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class DefaultWideningSuite extends FunSuite {
  val BoxDouble = BoxDoubleDomain()
  test ("delayed widening for boxes") {
    val d1 = BoxDouble(Array(0),Array(1))
    val wd = DefaultWidening
    val d2 = BoxDouble(Array(1),Array(2))
    val d3 = wd(d1,d2)
    assertResult ( BoxDouble(Array(0),Array(Double.PositiveInfinity)) ) { d3 }
  }
}