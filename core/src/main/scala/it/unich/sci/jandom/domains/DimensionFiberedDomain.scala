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

package it.unich.sci.jandom.domains

/**
 * A `DimensionFiberedDomain` is a domain whose fibers are characterized by a natural
 * number called dimension. Each property is endowed with operations
 * to increase or decrease dimension (i.e., add or remove variables). It is similar
 * to the concept of cylindric algebra, but operations are defined only for elements
 * of the same dimension.
 *
 * @author Gianluca Amato <gamato@unich.it>
 */

trait DimensionFiberedDomain extends AbstractDomain {
  type Property <: DimensionFiberedProperty[Property]

  /**
   * Returns the top element of the given dimension
   */
  def top(dimension: Int): Property

  /**
   * Returns the bottom element of the given dimension
   */
  def bottom(dimension: Int): Property
}
