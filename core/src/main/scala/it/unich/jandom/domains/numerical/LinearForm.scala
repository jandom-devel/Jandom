/**
 * Copyright 2013, 2016 Gianluca Amato
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

package it.unich.jandom.domains.numerical

import spire.math.Rational

/**
 * The LinearForm trait is used to represent a non-homogeneous linear form.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait LinearForm {
  /**
   * Returns the coefficients of the linear form. The first element is the constant term.
   */
  def coeffs: Seq[Rational]

  /**
   * Returns a sequence of pairs (i,ci) which are the indexes and values of non-zero
   * coefficients.
   */
  def pairs: Seq[(Int,Rational)]

  /**
   * Returns the constant term of the linear form.
   */
  def known: Rational

  /**
   * Returns the homogeneous coefficients of the linear form.
   */
  def homcoeffs: Seq[Rational]

  /**
   * Returns the homogeneous linear form associated
   */
  def hom: LinearForm

  /**
   * Returns true when the linear form is constant
   */
  def isConstant: Boolean

  /**
   * Returns thether the linear form is the zero constant
   */
  def isZero: Boolean

  /**
   * Returns the number of dimensions of the linear form
   */
  def dimension: Int

  /**
   * Return the opposite of the linear form.
   */
  def unary_-(): LinearForm

  /**
   * Add two linear forms.
   */
  def +(other: LinearForm): LinearForm

  /**
   * Subtract other from this.
   */
  def -(other: LinearForm): LinearForm

  /**
   * Multiply a linear form by a scalar.
   */
  def *(coeff: Rational): LinearForm

  /**
   * Multiply two linear forms if one of the two is a scalar. Returns
   * `None` if the multiplication is not linear.
   */
  def *(other: LinearForm): Option[LinearForm]

  /**
   * Divides a linear form by a scalar.
   * This requires `T` to be a fractional type.
   */
  def /(coeff: Rational): LinearForm

  /**
   * Divides two linear forms if the second one is a scalar. Returns
   * `None` otherwise.
   *  This requires `T` to be a fractional type.
   */
  def /(other: LinearForm): Option[LinearForm]

  /**
   * Returns the textual representation of a linear form.
   * @param vars symbolic names of variables in the linear form
   */
  def mkString(vars: Seq[String]): String

  /**
   * @inheritdoc
   * It is equivalent to `mkString` with variable names `v0`...`vn`
   */
  override def toString = mkString(Stream.from(0).map { "v" + _ })

  /**
   * This is used to store the PPL version of this linear form. It is declared
   * of type `AnyRef` so that it may be compiled even when PPL is not present.
   */
  var toPPL: AnyRef = null
}

/**
 * Factory object for the LinearForm class.
 */
object LinearForm {
  import scala.language.implicitConversions

  /**
   * Builds a linear form from given coefficients.
   * @param coeffs the coefficient of the linear form. The first coefficient
   * is the constant term.
   */
  def apply(coeffs: Rational*): LinearForm = DenseLinearForm(coeffs)

  /**
   * Builds a linear form from given non rational coefficients
   * @tparam T the type of coefficients we are using which may be converted to rationals
   * @param coeffs the coefficient of the linear form. The first coefficient
   * is the constant term.
   */
  def apply[T <% Rational](coeffs: T*): LinearForm = DenseLinearForm(coeffs map implicitly[T => Rational])

  /**
   * Builds a linear form given the non-null coefficients and constant term. Each pair
   * contains (i,ci) corresponds to the term ci*vi.
   * @param known the constant term of the linear form.
   * @param pairs a sequence of pairs (vi,ci) where vi is an index of a variable and ci the
   * corresponding coefficient.
   */
  def sparse(known: Rational, pairs: (Int,Rational)*): LinearForm = DenseLinearForm(known, pairs)

  /**
   * Builds the linear form corresponding to variable `vi`
   * @param i index of the variable vi
   */
  def v(i: Int): LinearForm = DenseLinearForm.v(i: Int)

  /**
   * Builds the constant linear form `c`
   */
  implicit def c(known: Rational): LinearForm = DenseLinearForm(List(known))

  /**
   * Builds the constant linear form `c` from a non-rational constant
   */
  implicit def c[T <% Rational](known: T): LinearForm = DenseLinearForm(List(known))
}
