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

import it.unich.sci.jandom.domains.DimensionFiberedProperty

/**
 * Base class for numerical properties and their operations.
 *
 * @tparam Property the property type we attach to and provide numerical operations.
 * @author Gianluca Amato <gamato@unich.it>
 * @note Most of the operations accepting variables as parameters have default values of `dimension-1` or `dimension-2`.
 *
 * @define PPL [[http://bugseng.com/products/ppl/ PPL]]
 * @define APRON [[http://apron.cri.ensmp.fr/library/ APRON]]
 * @define NOTEN `n` should be within `0` and `dimension-1`.
 * @define TODOGEN it should be generalized to linear forms over arbitrary types.
 * @define ILLEGAL IllegalArgumentException if parameters are not correct.
 */

trait NumericalProperty[Property <: NumericalProperty[Property]] extends DimensionFiberedProperty[Property] {
  this: Property =>

  /**
   * Non deterministic assignment (also called `forget` operator).
   * @note $NOTEN
   * @param n the variable to which non-deterministic assignment should be applied.
   */
  def nonDeterministicAssignment(n: Int = dimension - 1): Property

  /**
   * Linear assignment over an abstract object of the form `x(n) = x*coeff+known`.
   * @todo $TODOGEN
   * @param n the variable to be reassigned.
   * @param coeff the homogeneous coefficients.
   * @note $NOTEN
   * @note `coeff` should have at least `dimension` elements
   * @param known the in-homogeneous coefficient.
   */
  def linearAssignment(n: Int, coeff: Array[Double], known: Double): Property

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
   * Determines an upper bound of a linear form in the numerical object.
   * @param coeff the homogeneous coefficients of the linear form.
   * @param known the in-homogeneous coefficient.
   * @return an upper bound of the linear form
   */
  def minimize(coeff: Array[Double], known: Double): Double

  /**
   * Determines a lower bound of a linear form in the numerical object.
   * @param coeff the homogeneous coefficients of the linear form.
   * @param known the in-homogeneous coefficient.
   * @return a lower bound of the linear form
   */
  def maximize(coeff: Array[Double], known: Double): Double

  /**
   * Given a linear form, determines if there is a value 'c' such that the  linear form
   *  always evaluates to c in the numerical object.
   * @param coeff the homogeneous coefficients of the linear form.
   * @param known the in-homogeneous coefficient.
   * @return `Some(c)` if such a value exists, `None` otherwise.
   */
  def frequency(coeff: Array[Double], known: Double): Option[Double]

  /*
   * Now some concrete methods, which may be overriden in subclasses for
   * optimization purpose.
   */

  /**
   * Constant assignment to a variable. The standard implementation calls
   * linearAssignment, but it may be overriden in subclasses to optimize speed.
   * @note $NOTEN
   */
  def constantAssignment(n: Int = dimension - 1, c: Double) =
    linearAssignment(n, Array.fill(dimension)(0.0), c)

  /**
   * Assignment of a variable to another variable.
   * @note $NOTEN
   * @note `source` should be within `0` and `dimension-1`.
   */
  def variableAssignment(n: Int = dimension - 1, m: Int) = {
    require(m < dimension)
    val v = Array.fill(dimension)(0.0)
    v(m) = 1
    linearAssignment(n, v, 0)
  }

  /**
   * Assignments of the kind vn = vn + vm.  The standard implementation calls
   * linearAssignment, but it may be overriden in subclasses to optimize speed.
   * @note $NOTEN
   * @note `m` should be within `0` and `dimension-1`.
   */
  def variableAdd(n: Int = dimension - 2, m: Int = dimension - 1) = {
    require(n < dimension && m < dimension)
    val v = Array.fill(dimension)(0.0)
    v(n) = 1
    v(m) = 1
    linearAssignment(n, v, 0)
  }

  /**
   * Assignments of the kind vn = vn + vm.  The standard implementation calls
   * linearAssignment, but it may be overriden in subclasses to optimize speed.
   * @note $NOTEN
   * @note `m` should be within `0` and `dimension-1`.
   */
  def variableSub(n: Int = dimension - 2, m: Int = dimension - 1) = {
    require(n < dimension && m < dimension)
    val v = Array.fill(dimension)(0.0)
    v(n) = 1
    v(m) = -1
    linearAssignment(n, v, 0)
  }

  /**
   * Assignments of the kind vn = vn + c.  The standard implementation calls
   * linearAssignment, but it may be overriden in subclasses to optimize speed.
   * @note $NOTEN
   */
  def constantAdd(n: Int = dimension - 1, c: Double) = {
    require(n < dimension)
    val v = Array.fill(dimension)(0.0)
    v(n) = 1
    linearAssignment(n, v, c)
  }

  /**
   * Assignments of the kind vn = vn * vm.  The standard implementation determined
   * whether vn or vm is a constant, and use linearAssignment in such a case.
   * Otherwise, it resorts to a non deterministic assignment.
   * @note $NOTEN
   */
  def variableMul(n: Int = dimension - 2, m: Int = dimension - 1) = {
    // TODO: use method "frequency" here when it works for PPL
    val coeff = Array.fill(dimension)(0.0)
    coeff(n) = 1
    frequency(coeff, 0) match {
      case Some(c) =>
        coeff(n) = 0
        coeff(m) = c
        linearAssignment(n, coeff, 0)
      case None =>
        coeff(n) = 0
        coeff(m) = 1
        frequency(coeff, 0) match {
          case Some(c) =>
            coeff(n) = c
            coeff(m) = 0
            linearAssignment(n, coeff, 0)
          case None =>
            nonDeterministicAssignment(n)
        }
    }
  }

  /**
   * Assignments of the kind vn = vn / vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableDiv(n: Int = dimension - 2, m: Int = dimension - 1) = {
    nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn % vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableRem(n: Int = dimension - 2, m: Int = dimension - 1) = {
    nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn << vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableShl(n: Int = dimension - 2, m: Int = dimension - 1) = {
    nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn >> vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableShr(n: Int = dimension - 2, m: Int = dimension - 1) = {
    nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn >> vm for unsigned shift.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableUshr(n: Int = dimension - 2, m: Int = dimension - 1) = {
    nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn & vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableAnd(n: Int = dimension - 2, m: Int = dimension - 1) = {
    nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn | vm.  The standard implementation calls
   * nonDeterministicAssignment on vn
   * @note $NOTEN
   */
  def variableOr(n: Int = dimension - 2, m: Int = dimension - 1) = {
    nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = vn ^ vm.  The standard implementation calls
   * nonDeterministicAssignment on vn.
   * @note $NOTEN
   */
  def variableXor(n: Int = dimension - 2, m: Int = dimension - 1) = {
    nonDeterministicAssignment(n)
  }

  /**
   * Assignments of the kind vn = - vn.
   * @note NOTEN
   * @param n the dimension we want to negate
   * @return property with the negate dimension
   */
  def variableNeg(n: Int = dimension - 1) = {
    val coeff = Array.fill(dimension)(0.0)
    coeff(n) = -1
    linearAssignment(n, coeff, 0)
  }

  /**
   * The connect method is used for inter-procedural analysis. It takes two properties
   * such that the last `common` dimensions of `this` corresponds to the first `common`
   * dimension of `p`. It embeds both properties on a common space and intersect, then
   * remove the common dimensions.
   * @todo why not remove the private dimensions before connecting?
   * @param p property to connect with `this`
   * @param common number of common dimensions in `this` and `p`
   * @return the connected properties, according to the description above
   */
  def connect(p: Property, common: Int) = {
    val newprop = addVariables(p.dimension - common)
    val seq = (dimension - common until newprop.dimension) ++ (0 until dimension - common)
    val newp = p.addVariables(dimension - common).mapVariables(seq)
    (newprop intersection newp).delVariables(dimension - common to dimension - 1)
  }

  /**
   * Returns the string representation of the property. It calls `mkString` with the standard
   * variable names `v1` ... `vn`.
   */
  override def toString: String = mkString(for (i <- 0 until dimension) yield "v" + i)

}
