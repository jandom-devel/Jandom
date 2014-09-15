/**
 * Copyright 2013 Gianluca Amato
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

/**
 * The LinearForm trait is used to represent an in-homogeneous linear form.
 * @tparam T the type of the coefficients in the linear form.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait LinearForm[T] {
  /**
   * Returns the coefficients of the linear form. The first element is the constant term.
   */
  def coeffs: Seq[T]

  /**
   * Returns a sequence of pairs (i,ci) which are the indexes and values of non-zero
   * coefficients.
   */
  def pairs: Seq[(Int,T)]

  /**
   * Returns the constant term of the linear form.
   */
  def known: T

  /**
   * Returns the homogeneous coefficients of the linear form.
   */
  def homcoeffs: Seq[T]

  /**
   * Returns the number of dimensions of the linear form
   */
  def dimension: Int

  /**
   * Return the opposite of the linear form.
   */
  def unary_-(): LinearForm[T]

  /**
   * Add two linear forms.
   */
  def +(other: LinearForm[T]): LinearForm[T]

  /**
   * Subtract other from this.
   */
  def -(other: LinearForm[T]): LinearForm[T]

  /**
   * Multiply a linear form by a scalar.
   */
  def *(coeff: T): LinearForm[T]

  /**
   * Multiply two linear forms if one of the two is a scalar. Returns
   * `None` if the multiplication is not linear.
   */
  def *(other: LinearForm[T]): Option[LinearForm[T]]

  /**
   * Divides a linear form by a scalar.
   * This requires `T` to be a fractional type.
   */
  def /(coeff: T): LinearForm[T]

  /**
   * Divides two linear forms if the second one is a scalar. Returns
   * `None` otherwise.
   *  This requires `T` to be a fractional type.
   */
  def /(other: LinearForm[T]): Option[LinearForm[T]]

  /**
   * Returns a linear form over doubles
   */
  def toDouble: LinearForm[Double]

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
   * Builds a linear form given the coefficients.
   * @param coeffs the coefficient of the linear form. The first coefficient
   * is the constant term.
   */
  def apply[T: Numeric](coeffs: T*) = DenseLinearForm(coeffs)

  /**
   * Builds a linear form given the non-null coefficients and constant term. Each pair
   * contains (i,ci) corresponds to the term ci*vi. To ease the use of the LinearForm
   * object, the first pair is in the parameter `firstpair`, while the remaining
   * pairs are in the `restpair` parameter.
   * @param known the constant term of the linear form.
   */
  def apply[T: Numeric](known: T, firstpair: (Int,T), restpairs: (Int,T)*) = DenseLinearForm(known,firstpair +: restpairs)

  /**
   * Builds the linear form `vi`
   * @param i index of the variable vi
   */
  def v[T: Numeric](i: Int) = DenseLinearForm.v(i: Int)

  /**
   * Builds the constant linear form `c`
   */
  implicit def c[T: Numeric](known: T) = DenseLinearForm(Seq(known))
}
