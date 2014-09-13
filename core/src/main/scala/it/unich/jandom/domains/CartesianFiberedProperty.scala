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

/**
 * This class represents a single element in a CartesianFiberedDomain.
 * Properties are partitioned in fibers. Binary operators are guaranteed to work when
 * both elements are part of the same fiber. Finally, properties are immutable.
 *
 * Analogously to `isTop`, `isBottom` and `isEmpty`, 
 *
 * @define NOTEFIBER `this` and `that` should be element of the same fiber
 * @tparam Type the type of the components of a fiber.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait CartesianFiberedProperty[Type, Property <: CartesianFiberedProperty[Type, Property]] <: AbstractProperty[Property] {
  this: Property =>

  type Domain <: CartesianFiberedDomain { type FiberComponent = Type }

  /**
   * Returns the fiber identifier of the current property
   */
  def fiber: Domain#Fiber

  /**
   * Returns the dimension of the property, i.e., the number of variables in its fiber.
   */
  def dimension: Int
    
  /**
   * Returns true ONLY IF this is the top element on the fiber. A top element
   * is bigger than all the other elements, is neutral for intersection and
   * narrowing, and is absorbing for widening and union.
   */
  def isTop: Boolean

  /**
   * Returns true ONLY IF this is the bottom element of the fiber. The
   * opposite is not always true. A bottom element is smaller than all the other elements,
   * is neutral for union and widening and is absorbing for intersection and narrowing.
   * If `x.isEmpty` is true, the same happens for `x.isBottom`, but the opposite does
   * not always hold.
   */
  def isBottom: Boolean

  /**
   * Returns the top property on the same fiber as `this`.
   */
  def top: Property

  /**
   * Returns the bottom property on the same fiber as `this`.
   */
  def bottom: Property

  /**
   * Add a new variable of the given type.
   */
  def addVariable(t: Type): Property

  /**
   * Add new variables.
   * @param ts types of variables to add
   */
  def addVariables(ts: Seq[Type]): Property = {
    ts.foldLeft(this)((p, t) => p.addVariable(t))
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
    vs.foldRight(this)((i, p) => p.delVariable(i))
  }

  /**
   * Map variables according to a partial injective function.
   * @param rho partial injective function. Each dimension `i` is mapped to `rho(i)`. If `rho(i)` is
   * `-1`, then dimension i is removed
   */
  def mapVariables(rho: Seq[Int]): Property

  /**
   * The connect method is used for inter-procedural analysis. It takes two properties
   * such that the last `common` dimensions of `this` corresponds to the first `common`
   * dimension of `other`. The first represents the abstract state before calling a
   * procedure, the second represents the abstract state at the end of the procedure.
   * `connect` merge the two abstract states using a call-by-value semantics, and
   * remove the common dimension.
   * @todo why not remove the private dimensions before connecting?
   */
  def connect(other: Property, common: Int): Property

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
