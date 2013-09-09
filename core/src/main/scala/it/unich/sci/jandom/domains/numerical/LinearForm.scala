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

package it.unich.sci.jandom.domains.numerical

/**
 * The class LinearForm trait is used to represent an in-homogeneous linear form.
 * @tparam T the type of the coefficients in the linear form.
 * @author Gianluca Amato <amato@sci.unich.it>
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
}

/**
 * Factory object for the LinearForm class.
 */
object LinearForm {
  import scala.language.implicitConversions

  /**
   * Builds a linear form given the coefficients.
   * @param coeffs the coefficient of the linear form
   */
  implicit def apply[T: Numeric](coeffs: Seq[T]) = DenseLinearForm(coeffs)

  /**
   * Builds a linear form given the non-null coefficients and constant term
   * @param pairs the index and value of non-zero homogeneous coefficients. The indexes
   * in the pairs should be increasing.
   * @param known the constant term of the linear form.
   */
  implicit def apply[T: Numeric](pairs: Seq[(Int,T)], known: T) = DenseLinearForm(pairs, known)

  /**
   * Builds a constant linear form.
   * @param known the constant term
   */
  implicit def apply[T: Numeric](known: T) = DenseLinearForm(known)

  /**
   * Builds the linear form vi
   * @param i index of the variable vi
   */
  def v[T: Numeric](i: Int) = DenseLinearForm.v(i: Int)
}
