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

/**
 * The class DenseLinearForm is an implementation of the LinearForm trait using sequences. Hence, it is
 * quite convenient for dense linear forms.
 * @tparam T the type of the coefficients in the linear form. It should be endowed with an implicit Numeric[T] object.
 * @param coeffs the coefficients of the linear form. The first element is the known element.
 * @author Gianluca Amato <amato@sci.unich.it>
 */

class DenseLinearForm[T](val coeffs: Seq[T])(implicit numeric: Numeric[T]) extends LinearForm[T] {

  import numeric._

  def dimension = coeffs.length - 1

  def known = coeffs(0)

  def homcoeffs = coeffs.tail

  def pairs = for { (ci, i) <- homcoeffs.zipWithIndex ; if ci != 0 } yield (i,ci)

  /**
   * Equality between linear forms. Two linear forms are equal if their coefficients are the same and
   * are defined over the same environment.
   */
  override def equals(that: Any): Boolean = that match {
    case that: LinearForm[_] =>
      (coeffs zip that.coeffs) forall (tuple => tuple._1 == tuple._2)
    case _ => false
  }

  /**
   * Returns a LinearForm whose elements are the negations of the original one.
   */
  def unary_-(): LinearForm[T] = new DenseLinearForm(coeffs map (x => -x))

  /**
   * Addition of linear forms
   */
  def +(that: LinearForm[T]): LinearForm[T] = {
    new DenseLinearForm(coeffs.zipAll(that.coeffs, zero, zero) map (pair => pair._1 + pair._2))
  }

  /**
   * Subtraction of linear forms.
   */
  def -(that: LinearForm[T]): LinearForm[T] = this + (-that)

  /**
   * Multiplication of a scalar and a linear form.
   */
  def *(coeff: T): LinearForm[T] = new DenseLinearForm(coeffs map (_ * coeff))

  /**
   * Multiplication of linear forms.
   */
  def *(that: LinearForm[T]): Option[LinearForm[T]] = {
    if (coeffs.tails forall { _ == 0 })
      Some(new DenseLinearForm(that.coeffs map (_ * coeffs(0))))
    else if (that.coeffs.tails forall { _ == 0 })
      Some(new DenseLinearForm(coeffs map (_ * that.coeffs(0))))
    else None
  }

  /**
   * Returns the textual representation of a linear form.
   * @param vars symbolic names of variables in the linear form
   */
  def mkString(vars: Seq[String]): String = {
    import numeric._

    var first = true
    var index = 0
    var s = ""

    for (coeff <- coeffs) {
      val term = coeff match {
        case 0 => ""
        case 1 => if (index == 0) "1" else vars(index - 1)
        case -1 => if (index == 0) "-1" else "-" + vars(index - 1)
        case c => c.toString + (if (index == 0) "" else "*" + vars(index - 1))
      }
      if (coeff != 0) {
        if (first || coeff < zero) {
          s += term
          first = false
        } else if (coeff != 0)
          s += "+" + term
      }
      index += 1
    }
    if (s.isEmpty) "0" else s
  }

}

/**
 * Factory object for the LinearForm class.
 */
object DenseLinearForm {
  import scala.language.implicitConversions

  /**
   * Builds a dense linear form given the coefficients.
   * @param coeffs the coefficient of the linear form
   */
  def apply[T: Numeric](coeffs: Seq[T]) = new DenseLinearForm(coeffs)

  /**
   * Builds a dense linear form given the non-null coefficients and constant term
   * @param pairs the index and value of non-zero homogeneous coefficients. The indexes
   * in the pairs should be increasing.
   * @param known the constant term of the linear form.
   */
  def apply[T](pairs: Seq[(Int,T)], known: T)(implicit numeric: Numeric[T]) = {
    def pairsToCoeffs: (Seq[(Int,T)], Int) => List[T]  = { (pairs, n) =>
      if (pairs.isEmpty)
        Nil
      else {
        val pair = pairs.head
        if (pair._1 == n)
          pair._2 :: pairsToCoeffs(pairs.tail, n+1)
        else
          numeric.zero :: pairsToCoeffs(pairs, n+1)
      }
    }
    new DenseLinearForm(known :: pairsToCoeffs(pairs,0))
  }

  /**
   * Builds a constant dense linear form.
   * @param known the constant term
   */
  implicit def apply[T: Numeric](known: T) = new DenseLinearForm(Seq(known))

  /**
   * Builds the dense linear form vi
   * @param i index of the variable vi
   */
  def v[T](i: Int)(implicit numeric: Numeric[T]) = DenseLinearForm(Seq(i -> numeric.one),numeric.zero)
}
