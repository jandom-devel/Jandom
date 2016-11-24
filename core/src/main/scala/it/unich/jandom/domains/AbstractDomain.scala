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
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains

import it.unich.scalafix.lattice.Domain

/**
 * The base class for all abstract domains. An abstract domain is a collection of properties,
 * which may or may not be grouped into fibers.
 * @author Gianluca Amato <gianluca.amato@unich.it>
 */
trait AbstractDomain {
  /**
   * The type of the properties associated to this abstract domain.
   */
  type Property <: AbstractProperty[Property]

  /**
   * A non-empty set of widenings supported by the abstract domain. The first element is supposed to be
   * the standard widening and should correspond to the widening method of the ``it.unich.jandom.domains.AbstractProperty``
   * class.
   */
  def widenings: Seq[WideningDescription[Property]]

  /**
   * ScalaFixDomain is an instance of the ScalaFix type-class Domain for this abstract domain.
   */
  val ScalaFixDomain = new Domain[Property] {
    def lteq(x: Property, y: Property) = x <= y
    def tryCompare(x: Property, y: Property) = x tryCompareTo y
    def upperBound(x: Property, y: Property) = x union y
  }
}
