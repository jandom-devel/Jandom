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

package it.unich.sci.jandom.domains.numerical

import it.unich.sci.jandom.domains.AbstractProperty

/**
 * Base class for numerical properties and their operations. A concrete class `C` implementing a numerical
 * property should inherit from `NumericalProperty[C]`. Note that binary operations only works between
 * compatible properties, i.e. properties over vector spaces of the same dimension. Numerical
 * properties are immutable.
 *
 * Most of the operations accepting dimensions as parameters have default value of `dimension-1` or `dimension-2`.
 *
 * @tparam Property the property type we attach to and provide numerical operations.
 * @author Gianluca Amato <gamato@unich.it>
 * @define PPL [[http://bugseng.com/products/ppl/ PPL]]
 * @define APRON [[http://apron.cri.ensmp.fr/library/ APRON]]
 * @define NOTEN `n` should be within `0` and `dimension-1`.
 * @define TODOGEN it should be generalized to linear forms over arbitrary types.
 * @define ILLEGAL IllegalArgumentException if parameters are not correct.
 */

trait NumericalProperty[Property <: NumericalProperty[Property]] extends AbstractProperty[Property] {
  this: Property =>
  /**
   * Non deterministic assignment (also called `forget` operator).
   * @note $NOTEN
   * @param n the variable to which non-deterministic assignment should be applied.
   */
  def nonDeterministicAssignment(n: Int = dimension-1): Property

  /**
   * Linear assignment over an abstract object of the form `x(n) = x*coeff+known`.
   * @todo $TODOGEN
   * @param n the variable to be reassigned.
   * @param coeff the homogeneous coefficients.
   * @note $NOTEN
   * @note `coeff` should have at least `dimension` elements
   * @param known the in-homogeneous coefficient.
   */
  def linearAssignment(n:Int, coeff:Array[Double], known: Double): Property

  /**
   * Intersection with the half-plane `{ x |  coeff*x+known <= 0 }`.
   * @todo $TODOGEN
   * @param coeff the homogeneous coefficients.
   * @note `coeff` should have at least `dimension` elements
   * @param known the in-homogeneous coefficient.
   */
  def linearInequality(coeff: Array[Double], known: Double): Property

  /**
   * Intersection with the complements of a line `{ x |  coeff*x+known != 0 }`.
   * @todo $TODOGEN
   * @param coeff the homogeneous coefficients.
   * @note `coeff` should have at least dimension elements
   * @param known the in-homogeneous coefficient.
   */
  def linearDisequality(coeff: Array[Double], known: Double): Property

  /**
   * Add a new undetermined dimension.
   */
  def addDimension: Property

  /**
   * Delete a given dimension.
   * @param n the dimension to be suppressed.
   * @note $NOTEN
   */
  def delDimension(n: Int = dimension-1): Property

  /**
   * Map dimensions according to a partial injective function.
   * @param rho partial injective function. Each dimension `i` is mapped to `rho(i)`. If `rho(i)` is
   * `-1`, then dimension i is removed.
   */
  def mapDimensions(rho: Seq[Int]): Property

  /**
   * Returns the dimension of the environment space.
   * @return the dimension of the environment space.
   */
  def dimension: Int

  /**
   * Test of emptiness
   * @return whether the abstract object is empty.
   */
  def isEmpty: Boolean

  /**
   * Test for fullness.
   * @return whether the abstract object represents the full environment space.
   */
  def isFull: Boolean

  /**
   * Returns an empty object with the same `dimension` as `this`.
   */
  def empty: Property

  /**
   * Returns a full object with the same `dimension` as `this`.
   */
  def full: Property

  /**
   * @inheritdoc
   * Returns false since, if not otherwise specified, numerical domains have no top element.
   */
  def isTop = false

  /**
   * @inheritdoc
   * Returns false since, if not otherwise specified, numerical domains have no bottom element.
   */
  def isBottom = false

  /**
   * Returns a string representation of the property.
   * @param vars an array with the name of the variables
   * @return a sequence of strings. The idea is that each string is an atomic piece of information
   * which should be printed out together, while different strings may be also printed out
   * separately.
   */
  def mkString(vars: IndexedSeq[String]): Seq[String]

  /*
   * Now some concrete methods follow, which may be overriden in subclasses for
   * optimization purpose.
   */

  /**
   * Constant assignment to a variable. The standard implementation calls
   * linearAssignment, but it may be overriden in subclasses to optimize speed.
   * @note $NOTEN
   */
  def constantAssignment(n: Int = dimension-1, c: Double) =
    linearAssignment(n, Array.fill(dimension)(0.0), c)

  /**
   * Assignment of a variable to another variable.
   * @note $NOTEN
   * @note `source` should be within `0` and `dimension-1`.
   */
  def variableAssignment(n: Int = dimension-1, m: Int)  = {
    require (m < dimension)
    val v = Array.fill(dimension)(0.0)
    v(m) = 1
    linearAssignment(n,v,0)
  }

  /**
   * Assignments of the kind vn = vn + vm.  The standard implementation calls
   * linearAssignment, but it may be overriden in subclasses to optimize speed.
   * @note $NOTEN
   * @note `m` should be within `0` and `dimension-1`.
   */
  def variableAdd(n: Int = dimension-2, m: Int = dimension-1) = {
    require (n < dimension && m < dimension)
    val v = Array.fill(dimension)(0.0)
    v(n) = 1
    v(m) = 1
    linearAssignment(n,v,0)
  }

  /**
   * Assignments of the kind vn = vn + vm.  The standard implementation calls
   * linearAssignment, but it may be overriden in subclasses to optimize speed.
   * @note $NOTEN
   * @note `m` should be within `0` and `dimension-1`.
   */
  def variableSub(n: Int = dimension-2, m: Int = dimension-1) = {
    require (n < dimension && m < dimension)
    val v = Array.fill(dimension)(0.0)
    v(n) = 1
    v(m) = -1
    linearAssignment(n,v,0)
  }

  /**
   * Assignments of the kind vn = vn + c.  The standard implementation calls
   * linearAssignment, but it may be overriden in subclasses to optimize speed.
   * @note $NOTEN
   */
  def constantAdd(n: Int = dimension-1, c: Double) = {
    require (n < dimension)
    val v = Array.fill(dimension)(0.0)
    v(n) = 1
    linearAssignment(n,v,c)
  }

  /**
   * Assignments of the kind vn = vn * vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableMul(n: Int = dimension-2, m: Int= dimension-1)  = {
     nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn / vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableDiv(n: Int = dimension-2, m: Int= dimension-1)  = {
     nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn % vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableRem(n: Int = dimension-2, m: Int= dimension-1)  = {
     nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn << vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableShl(n: Int = dimension-2, m: Int= dimension-1)  = {
     nonDeterministicAssignment(n)
  }

    /**
   * Assignments of the kind vn = vn >> vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableShr(n: Int = dimension-2, m: Int= dimension-1)  = {
     nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn >> vm for unsigned shift.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableUshr(n: Int = dimension-2, m: Int= dimension-1)  = {
     nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn & vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableAnd(n: Int = dimension-2, m: Int= dimension-1)  = {
     nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn | vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableOr(n: Int = dimension-2, m: Int= dimension-1)  = {
     nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn ^ vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableXor(n: Int = dimension-2, m: Int= dimension-1)  = {
     nonDeterministicAssignment(n)
  }

  def variableNeg(n: Int = dimension-1) = {
    require (n >=0 && n < dimension)
    val coeff = Array.fill(dimension)(0.0)
    coeff(n) = -1
    linearAssignment(n, coeff, 0)
  }

  /**
   * Add many undetermined dimensions.
   * @param n number of dimensions to add.
   */
  def addDimension(n: Int): Property = {
    require (n >= 0)
    (0 until n).foldLeft(this) { (prop,_) => prop.addDimension }
  }

  /**
   * Returns the string representation of the property. It calls `mkString` with the standard
   * variable names `v1` ... `vn`.
   */
  override def toString: String = "[ " + (mkString( for (i <- 0 until dimension) yield "v"+i )).mkString(" , ") + " ]"

}
