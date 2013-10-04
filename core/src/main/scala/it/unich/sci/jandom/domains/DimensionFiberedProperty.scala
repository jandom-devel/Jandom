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
 * A `DimensionFiberedProperty` is an element of a `DimensionFiberedDomain`. Each fiber is characterized by
 * a natural number called `size`, which is the number of dimensions in the object. There are many methods
 * to add and remove dimensions.
 * @tparam Property the real class we are endowing with the DimensionFiberedProperty quality.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait DimensionFiberedProperty[Property <: DimensionFiberedProperty[Property]] <: AbstractProperty[Property] {
  this: Property =>

  type Domain <: DimensionFiberedDomain

  /**
   * Returns the dimension of the property.
   */
  def dimension: Int

  /**
   * Add a new variable.
   */
  def addVariable(): Property

  /**
   * Add `m` new variables.
   * @note `m` should be positive
   */
  def addVariables(m: Int): Property = {
    require (m >= 0)
    (0 until m).foldLeft(this) ( (p,i) => p.addVariable() )
  }

  /**
   * Remove the variable `v`, appropriately projecting the property.
   * @param v variable to remove
   * @note `v` should be between 0 and `dimension`-1
   */
  def delVariable(v: Int = dimension - 1): Property

  /**
   * Remove variables in the `vs` range.
   * @param vs the range of variables to remove
   */
  def delVariables(vs: Range): Property = {
    vs.foldRight(this) ( (i,p) => p.delVariable(i) )
  }

  /**
   * Map variables according to a partial injective function.
   * @param rho partial injective function. Each dimension `i` is mapped to `rho(i)`. If `rho(i)` is
   * `-1`, then dimension i is removed
   */
  def mapVariables(rho: Seq[Int]): Property

  /**
   * Returns a string representation of the abstract property.
   * @param vars an array with the name of the variables in the environment
   */
  def mkString(vars: Seq[String]): String

  /**
   * Returns the string representation of the property. It calls `mkString` with the standard
   * variable names `v1` ... `vn`.
   */
  override def toString: String = mkString(for (i <- 0 until dimension) yield "v" + i)
}
