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

package it.unich.jandom.domains.numerical.ppl

import parma_polyhedra_library.Double_Box
import parma_polyhedra_library.Degenerate_Element
import it.unich.jandom.domains.numerical.NumericalDomain

/**
 * The domain for possibly opened box over doubles implemented within $PPL.
 * It is declared private so that we can be sure to build a single instance of it. We do not
 * directly use Scala objects because we want to keep a uniform behaviour with other
 * non-singleton abstract domains.
 *
 * This class could me removed in favor of `PPLDomain[C_Polyehdron]` and the analogous macro-based
 * domain, but we keep it here since it the simplest PPL class in which to tinker around.
 * @param pplbox an object of class `Double_Box` which is the $PPL wrapped object.
 * @author Gianluca Amato <gamato@unich.it>
 */
class PPLBoxDoubleDomain extends NumericalDomain {
  PPLInitializer

  type Property = PPLBoxDouble

  def top(n: Int): PPLBoxDouble = {
    val pplbox = new Double_Box(n, Degenerate_Element.UNIVERSE)
    new PPLBoxDouble(pplbox)
  }

  def bottom(n: Int): PPLBoxDouble = {
    val pplbox = new Double_Box(n, Degenerate_Element.EMPTY)
    new PPLBoxDouble(pplbox)
  }
}

/**
 * The companion object to `PPLBoxDoubleDomain`
 */
object PPLBoxDoubleDomain {
  def apply() = v
  private val v = new PPLBoxDoubleDomain()
}
