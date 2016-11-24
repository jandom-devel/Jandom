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

import it.unich.scalafix.Box

/**
 * The description of a widening.
 * @tparam the type of objects on which the widening operates.
 * @param box the box which implements the widening.
 * @param name short name of the widening.
 * @param description long description of the widening.
 */
class WideningDescription[Property](val name: String, val description: String, val box: Box[Property]) {
  /**
   * Application of widening is equivalent to the application of the underlying box.
   */
  def apply(a: Property, b: Property) = box(a, b)
}

/**
 * Companion object for widening description. It contains factory methods.
 */
object WideningDescription {
  /**
   * Builds a widening description.
   * @tparam the type of objects on which the widening operates.
   * @param box the box which implements the widening.
   * @param name short name of the widening.
   * @param description long description of the widening.
   */
  def apply[Property](name: String, description: String, box: Box[Property]) =
    new WideningDescription(name, description, box)

  /**
   * Returns the description of the standard widening for abstract property `Property`.
   */
  def default[Property <: AbstractProperty[Property]] =
    WideningDescription("default", "The default widening.", Box { (a: Property, b: Property) => a widening b })
}
