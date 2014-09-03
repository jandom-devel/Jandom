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
 * A `DimensionFiberedProperty` is an element of a `DimensionFiberedDomain`.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait DimensionFiberedProperty[Property <: DimensionFiberedProperty[Property]] <: CartesianFiberedProperty[Unit,Property] {
  this: Property =>

  type Domain <: DimensionFiberedDomain

  /**
   * Add a new variable.
   */
  def addVariable(): Property

  /**
   * Add `m` new variables.
   * @note `m` should be positive
   */
  def addVariables(m: Int): Property = {
    require(m >= 0)
    (0 until m).foldLeft(this)((p, i) => p.addVariable())
  }

  def fiber = Seq.fill[Unit](dimension)(())

  def addVariable(t: Unit) = addVariable()
}
