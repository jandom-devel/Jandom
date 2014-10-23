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

package it.unich.jandom.domains.numerical

import it.unich.jandom.domains.DimensionFiberedProperty

/**
 * Base class for numerical properties and their operations.
 *
 * @tparam Property the real class we are endowing with the NumericalProperty quality.
 * @note Most of the operations accepting variables as parameters have default values of `dimension-1` or `dimension-2`.
 *
 * @define STDINST The standard implementation uses `linearAssignment`, but may be overriden in subclasses for greater efficiency.
 * @define NDAINST The standard implementation uses `nonDeterministicAssignment`.
 * @define PPL [[http://bugseng.com/products/ppl/ PPL]]
 * @define APRON [[http://apron.cri.ensmp.fr/library/ APRON]]
 * @define NOTEN `n` should be within `0` and `dimension-1`.
 * @define TODOGEN it should be generalized to linear forms over arbitrary types.
 * @define ILLEGAL IllegalArgumentException if parameters are not correct.
 *
 * @author Gianluca Amato <gamato@unich.it>
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
   * Linear assignment `xn := lf`.
   * @todo $TODOGEN
   * @param n the variable to be assigned.
   * @param lf the linear form determining the assignment.
   * @note $NOTEN
   */
  def linearAssignment(n: Int, lf: LinearForm[Double]): Property

  /**
   * Intersection with the half-plane `lf <= 0`.
   * @todo $TODOGEN
   */
  def linearInequality(lf: LinearForm[Double]): Property

  /**
   * Intersection with `lf != 0`.
   * @todo $TODOGEN
   */
  def linearDisequality(lf: LinearForm[Double]): Property

  /**
   * Determines a lower bound of a linear form in the numerical object.
   * @todo $TODOGEN
   * @return a lower bound of the linear form. It is not guaranteed to be the
   * greatest lower bound  It is -∞ when the linear form is unbounded.
   */
  def minimize(lf: LinearForm[Double]): Double

  /**
   * Determines an upper bound of a linear form in the numerical object.
   * @todo $TODOGEN
   * @return an upper bound of the linear form. It is not guaranteed to be the
   * least upper bound  It is +∞ when the linear form is unbounded.
   */
  def maximize(lf: LinearForm[Double]): Double

  /**
   * Given a linear form, determines if there is a value 'c' such that the  linear form
   * always evaluates to c in the numerical object. The name comes from the PPL method
   * with the same name.
   * @todo $TODOGEN
   * @return `Some(c)` if such a value exists, `None` otherwise.
   */
  def frequency(lf: LinearForm[Double]): Option[Double]

  /**
   * Returns a set of constraints which are bounding hyperplanes for the property.
   * The constraints may be redundant. Moreover, they exactly describe the property
   * only when `isPolyhedral` is true.
   */
  def constraints: Seq[LinearForm[Double]]

  /**
   * Returns whether the `constraints` methods returns an exact representation of the
   * property.
   */
   def isPolyhedral: Boolean

  /*
   * Now some concrete methods, which may be overriden in subclasses for
   * optimization purpose.
   */

  def connect(other: Property, common: Int) = {
    val newprop = addVariables(other.dimension - common)
    val seq = (dimension - common until newprop.dimension) ++ (0 until dimension - common)
    val newp = other.addVariables(dimension - common).mapVariables(seq)
    (newprop intersection newp).delVariables(dimension - common to dimension - 1)
  }

  /**
   * Constant assignment to a variable `vn := c`.
   * @note $STDINST
   * @note $NOTEN
   */
  def constantAssignment(n: Int = dimension - 1, c: Double) =
    linearAssignment(n, c)

  /**
   * Assignment of a variable to another variable `vn := vm`.
   * @note $STDINST
   * @note $NOTEN
   * @note `m` should be within `0` and `dimension-1`.
   */
  def variableAssignment(n: Int = dimension - 1, m: Int) =
    linearAssignment(n, LinearForm.v(m))

  /**
   * Assignment of the kind `vn = vn + vm`.
   * @note $STDINST
   * @note $NOTEN
   * @note `m` should be within `0` and `dimension-1`.
   */
  def variableAdd(n: Int = dimension - 2, m: Int = dimension - 1) =
    linearAssignment(n, LinearForm.v[Double](n) + LinearForm.v[Double](m))

  /**
   * Assignments of the kind `vn = vn - vm`.
   * @note $STDINST
   * @note $NOTEN
   * @note `m` should be within `0` and `dimension-1`.
   */
  def variableSub(n: Int = dimension - 2, m: Int = dimension - 1) =
    linearAssignment(n, LinearForm.v[Double](n) - LinearForm.v[Double](m))

  /**
   * Assignments of the kind `vn = vn + c`.
   * @note $STDINST
   * @note $NOTEN
   */
  def constantAdd(n: Int = dimension - 1, c: Double) =
    linearAssignment(n, LinearForm(c, n -> 1.0))

  /**
   * Assignments of the kind `vn = vn * vm`.  The standard implementation determines
   * whether `vn` or `vm` is a constant, and use linearAssignment in such a case.
   * Otherwise, it resorts to a non deterministic assignment.
   * @note $NOTEN
   */
  def variableMul(n: Int = dimension - 2, m: Int = dimension - 1) = {
    frequency(LinearForm.v(n)) match {
      case Some(c) => linearAssignment(n, LinearForm(0, m -> c))
      case None => frequency(LinearForm.v(m)) match {
        case Some(c) => linearAssignment(n, LinearForm(0, n -> c))
        case None => nonDeterministicAssignment(n)
      }
    }
  }

  /**
   * Assignments of the kind `vn = vn / vm`.  The standard implementation calls determines
   * whether is a constant, and use linearAssignment in such a case. Otherwise, it
   * resorts to a non deterministic assignment.
   * @note $NOTEN
   */
  def variableDiv(n: Int = dimension - 2, m: Int = dimension - 1) = {
    frequency(LinearForm.v(m)) match {
      case Some(c) => if (c != 0) linearAssignment(n, LinearForm(0, n -> 1 / c)) else bottom
      case None => nonDeterministicAssignment(n)
    }
  }

  /**
   * Assignments of the kind `vn = vn % vm`.
   * @note $NDAINST
   * @note $NOTEN
   */
  def variableRem(n: Int = dimension - 2, m: Int = dimension - 1) =
    nonDeterministicAssignment(n)

  /**
   * Assignments of the kind `vn = vn << vm`.
   * @note $NDAINST
   * @note $NOTEN
   */
  def variableShl(n: Int = dimension - 2, m: Int = dimension - 1) =
    nonDeterministicAssignment(n)

  /**
   * Assignments of the kind `vn = vn >> vm`.
   * @note $NDAINST
   * @note $NOTEN
   */
  def variableShr(n: Int = dimension - 2, m: Int = dimension - 1) =
    nonDeterministicAssignment(n)

  /**
   * Assignments of the kind `vn = vn >> vm` for unsigned shift.
   * @note $NDAINST
   * @note $NOTEN
   */
  def variableUshr(n: Int = dimension - 2, m: Int = dimension - 1) =
    nonDeterministicAssignment(n)

  /**
   * Assignments of the kind `vn = vn & vm`.
   * @note $NDAINST
   * @note $NOTEN
   */
  def variableAnd(n: Int = dimension - 2, m: Int = dimension - 1) =
    nonDeterministicAssignment(n)

  /**
   * Assignments of the kind `vn = vn | vm`.
   * @note $NDAINST
   * @note $NOTEN
   */
  def variableOr(n: Int = dimension - 2, m: Int = dimension - 1) =
    nonDeterministicAssignment(n)

  /**
   * Assignments of the kind `vn = vn ^ vm`.
   * @note $NDAINST
   * @note $NOTEN
   */
  def variableXor(n: Int = dimension - 2, m: Int = dimension - 1) =
    nonDeterministicAssignment(n)

  /**
   * Assignments of the kind `vn = - vn`.
   * @note $STDINST
   * @note $NOTEN
   */
  def variableNeg(n: Int = dimension - 1) =
    linearAssignment(n, LinearForm(0, n -> -1.0))

}
