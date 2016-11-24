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

package it.unich.jandom.domains

/**
 * A `DimensionFiberedDomain` is an abstract domain where each fiber is identified
 * by a natural number. It is implemented here as a sub-type of `CartesianFiberedDomain`,
 * mapping fiber `n` to a sequence of `n` elements of type Unit.
 * @author Gianluca Amato <gamato@unich.it>
 */

trait DimensionFiberedDomain extends CartesianFiberedDomain {
  type FiberComponent = Unit

  type Property <: DimensionFiberedProperty[Property]

  /**
   * Returns the top element of the given dimension.
   */
  def top(dimension: Int): Property

  /**
   * Returns the bottom element of the given dimension.
   */
  def bottom(dimension: Int): Property

  def top(f: Fiber) = top(f.length)

  def bottom(f: Fiber) = bottom(f.length)
}
