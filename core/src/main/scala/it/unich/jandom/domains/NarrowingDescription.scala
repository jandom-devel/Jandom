/**
  * Copyright 2017 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.scalafix.Box

/**
  * The description of a narrowing.
  * @tparam Property type of objects on which the narrowing operates.
  * @param name short name of the narrowing.
  * @param description long description of the narrowing.
  * @param box the box which implements the narrowing.
  */
class NarrowingDescription[Property](val name: String, val description: String, val box: Box[Property]) {
  /**
    * Application of narrowing description is equivalent to the application of the underlying box.
    */
  def apply(a: Property, b: Property) = box(a, b)
}

/**
  * Companion object for narrowing description. It contains factory methods.
  */
object NarrowingDescription {
  /**
    * Builds a narrowing description.
    * @tparam Property type of objects on which the narrowing operates.
    * @param name short name of the narrowing.
    * @param description long description of the narrowing.
    * @param box the box which implements the narrowing.
    */
  def apply[Property](name: String, description: String, box: Box[Property]) =
    new NarrowingDescription(name, description, box)

  /**
    * Returns the description of the standard narrowing for abstract property `Property`.
    */
  def default[Property <: AbstractProperty[Property]] =
    NarrowingDescription("default", "The default narrowing.", Box { (a: Property, b: Property) => a narrowing b })
}
