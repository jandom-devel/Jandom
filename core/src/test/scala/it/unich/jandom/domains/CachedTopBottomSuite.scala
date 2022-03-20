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
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains

import it.unich.jandom.domains.numerical.BoxDoubleDomain
import org.scalatest.funsuite.AnyFunSuite

/**
 * The test suite for the `CachedTopBottom` trait.
 * @author Gianluca Amato <gamato@unich.it>
 */
class CachedTopBottomSuite extends AnyFunSuite  {
   val cached = new BoxDoubleDomain(false) with CachedTopBottom

   test("cached bottoms always returns the same objects") {
     val dimensions = Seq(1,2,5)
     val bottoms = dimensions map { cached.bottom(_) }
     val rebottoms = dimensions map { cached.bottom(_) }
     for ((x,y) <- bottoms zip rebottoms) assert(x eq y)
   }

   test("cached tops always returns the same objects") {
     val dimensions = Seq(1,2,5)
     val tops = dimensions map { cached.top(_) }
     val retops = dimensions map { cached.top(_) }
     for ((x,y) <- tops zip retops) assert(x eq y)
   }
}
