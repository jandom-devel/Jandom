/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.scalafix.Box
import parma_polyhedra_library.Degenerate_Element
import parma_polyhedra_library.Double_Box

/**
 * The domain for possibly opened box over doubles implemented within $PPL.
 * It is declared private so that we can be sure to build a single instance of it. We do not
 * directly use Scala objects because we want to keep a uniform behaviour with other
 * non-singleton abstract domains.
 *
 * This class could me removed in favor of `PPLDomain[C_Polyehdron]` and the analogous macro-based
 * domain, but we keep it here since it the simplest PPL class in which to tinker around.
 * @author Gianluca Amato <gamato@unich.it>
 */
class PPLBoxDoubleDomain extends NumericalDomain {
  PPLInitializer

  type Property = PPLBoxDouble

  val widenings = Seq(
    WideningDescription.default[Property],
    WideningDescription("CC76", "The widening on boxes described in Cousot & Cousot '76",
      Box { (a: Property, b: Property) => a CC76Widening b }))

  def top(n: Int): Property = {
    val pplbox = new Double_Box(n, Degenerate_Element.UNIVERSE)
    new Property(pplbox)
  }

  def bottom(n: Int): Property = {
    val pplbox = new Double_Box(n, Degenerate_Element.EMPTY)
    new Property(pplbox)
  }
}

/**
 * The companion object to `PPLBoxDoubleDomain`
 */
object PPLBoxDoubleDomain {
  def apply() = v
  private val v = new PPLBoxDoubleDomain()
}
