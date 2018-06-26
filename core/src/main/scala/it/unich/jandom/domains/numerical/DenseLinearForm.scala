/**
 * Copyright 2013, 2016 Gianluca Amato <gamato@unich.it>
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
 * The class DenseLinearForm is an implementation of the LinearForm trait using sequences. Hence, it is
 * quite convenient for dense linear forms.
 * @param coeffs the coefficients of the linear form. The first element is the known element.
 * @author Gianluca Amato <gamato@unich.it>
 */

class DenseLinearForm[T](val coeffs: Seq[Rational]) extends LinearForm {

  def dimension = coeffs.length - 1

  def known = coeffs(0)

  def homcoeffs = coeffs.tail

  def hom = DenseLinearForm(Rational.zero +: coeffs.tail)

  def isConstant = homcoeffs.forall( _.isZero )

  def isZero = isConstant && known.isZero

  def pairs = for { (ci, i) <- homcoeffs.zipWithIndex ; if ! ci.isZero } yield (i,ci)

  def unary_-(): LinearForm = new DenseLinearForm(coeffs map (x => -x))

  def +(that: LinearForm): LinearForm = {
    new DenseLinearForm(coeffs.zipAll(that.coeffs, Rational.zero, Rational.zero) map (pair => pair._1 + pair._2))
  }

  def -(that: LinearForm): LinearForm = this + (-that)

  def *(coeff: Rational): LinearForm = new DenseLinearForm(coeffs map (_ * coeff))

  def *(that: LinearForm): Option[LinearForm] = {
    if (homcoeffs forall { _.isZero })
      Option(new DenseLinearForm(that.coeffs map (_ * coeffs(0))))
    else if (that.homcoeffs forall { _.isZero })
      Option(new DenseLinearForm(coeffs map (_ * that.coeffs(0))))
    else
      Option.empty
  }

  def /(coeff: Rational): LinearForm = {
    new DenseLinearForm(coeffs map (_ / coeff))
  }

  def /(that: LinearForm): Option[LinearForm] = {
    if ((that.homcoeffs forall { _.isZero }) && ! that.known.isZero )
      Option(new DenseLinearForm(coeffs map ( _ / that.known)))
    else
      Option.empty
  }

  /**
   * Returns the textual representation of a linear form.
   * @param vars symbolic names of variables in the linear form
   */
  def mkString(vars: Seq[String]): String = {
    var first = true
    var index = 0
    var s = ""

    for (coeff <- coeffs) {
      val term = coeff match {
        case Rational.zero => ""
        case Rational.one => if (index == 0) "1" else vars(index - 1)
        case c if c == -Rational.one => if (index == 0) "-1" else "-" + vars(index - 1)
        case c => c.toString + (if (index == 0) "" else "*" + vars(index - 1))
      }
      if (! coeff.isZero) {
        if (first || coeff < Rational.zero) {
          s += term
          first = false
        } else
          s += "+" + term
      }
      index += 1
    }
    if (s.isEmpty) "0" else s
  }

}

/**
 * Factory object for the DenseLinearForm class.
 */
object DenseLinearForm {
  /**
   * Builds a dense linear form given the coefficients.
   * @param coeffs the coefficient of the linear form
   */
  def apply(coeffs: Seq[Rational]) = new DenseLinearForm(coeffs)

  /**
   * Builds a dense linear form given the non-null coefficients and constant term
   * @param pairs the index and value of non-zero homogeneous coefficients. The indexes
   * in the pairs should be increasing.
   * @param known the constant term of the linear form.
   */
  def apply(known: Rational, pairs: Seq[(Int,Rational)]) = {
    def pairsToCoeffs: (Seq[(Int,Rational)], Rational) => List[Rational]  = { (pairs, n) =>
      if (pairs.isEmpty)
        Nil
      else {
        val pair = pairs.head
        if (pair._1 == n)
          pair._2 :: pairsToCoeffs(pairs.tail, n+1)
        else
          Rational.zero :: pairsToCoeffs(pairs, n+1)
      }
    }
    new DenseLinearForm(known :: pairsToCoeffs(pairs, Rational.zero))
  }

  /**
   * Builds the dense linear form vi
   * @param i index of the variable vi
   */
  def v(i: Int) = DenseLinearForm(Rational.zero, List(i -> Rational.one))
}
