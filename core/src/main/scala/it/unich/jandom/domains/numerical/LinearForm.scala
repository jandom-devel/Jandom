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

/**
  * Inequalities are for human-readable pretty-printing of constraints
  */
object Inequalities {

  trait Inequality {
    def mkString(vars: Seq[String]) : String
    def contains(x: Rational) : Boolean
  }

  def homCoeffToString (homcoeffs : Seq[Rational], vars : Seq[String]) : String = {
    homcoeffs.zip(0 to homcoeffs.length-1).map((k : (Rational, Int)) =>
      k match { case (x,y) =>
        if (x != 0) {
          ((
            if (x != 1)
            {
              if (x != -1)
              { (x)+"*" }
              else { "-" }
            }
            else
            {""}
          )+vars(y)).replace("+","")
        }
        else { "" }
      }
    ).
      filter(_ != "").
      mkString("+").
      replace("+-", "-")
  }

  case class Eq(c: Rational, homcoeffs: Seq[Rational]) extends Inequality {
    def contains(x: Rational) = x == c
    def mkString(vars: Seq[String]) = {
      // Rewrite eq for optimal polarity, i.e. with minimum number of -
      val negs = (homcoeffs :+ c).filter(_ < 0).length
      val poss = (homcoeffs :+ c).filter(_ > 0).length
      if (negs > poss) {
        // Too many - signs, flip
        homCoeffToString(homcoeffs.map(_ * -1), vars)+" = "+(-c)
      } else {
        homCoeffToString(homcoeffs, vars)+" = "+(c)
      }
    }
  }

  case class Ineq(c: Rational, homcoeffs: Seq[Rational]) extends Inequality {
    def contains(x: Rational) = x >= c
    def mkString(vars: Seq[String]) = {
      // Rewrite eq for optimal polarity, i.e. with minimum number of -
      val negs = (homcoeffs :+ c).filter(_ < 0).length
      val poss = (homcoeffs :+ c).filter(_ > 0).length
      if (negs > poss) {
        // Too many - signs, flip
        homCoeffToString(homcoeffs.map(_ * -1),vars)+" <= "+(-c)
      } else {
        homCoeffToString(homcoeffs,vars)+" >= "+c
      }
    }
  }

  case class Between(bottom: Rational, top: Rational, homcoeffs: Seq[Rational]) extends Inequality {
    assert(bottom <= top)
    def contains(x: Rational) = bottom >= x & x >= top
    def mkString(vars: Seq[String]) = {
      // Rewrite eq for optimal polarity, i.e. with minimum number of -
      val negs = (homcoeffs :+ top :+ bottom).filter(_ < 0).length
      val poss = (homcoeffs :+ top :+ bottom).filter(_ > 0).length
      if (negs > poss) {
        // Too many - signs, flip
        -top + " <= " + homCoeffToString(homcoeffs.map(_ * -1), vars)+" <= "+ -bottom
      } else {
        bottom + " <= " + homCoeffToString(homcoeffs, vars)+" <= " + top
      }
    }
  }

  def mergeIneq(i1 : Inequality, i2 : Inequality) :  Either[Inequality, (Inequality, Inequality)] = {
    i1 match {
      case Ineq(c1, hom1) =>
        i2 match {
          case Eq(c2, hom2) => {
            if (hom1 == hom2) {
              assert(i1.contains(c2))
              Left(i2)
            } else {
              if (hom1.map(_ * -1) == hom2) {
                assert(i2.contains(-c2))
                Left(i2)
              } else {
                // Incompatible coeffs
                Right((i1, i2))
              }
            }
          }
          case Ineq(c2, hom2) => {
            if (hom1 == hom2) {
              // hom1 >= c1, hom1 >= c2 => tighter bound is max(c1,c2)
              Left(Ineq(c1.max(c2), hom1))
            } else {
              if (hom1.map(_ * -1) == hom2) {
                // hom1 >= c1 => -hom1 <= -c1
                if (c2 == -c1) {
                  // hom2 <= c2 & hom2 >= c2 => hom2 == c2
                  Left((Eq(c1, hom1)))
                } else {
                  // hom2 <= -c1 & hom2 >= c2 => c2 <= hom2 <= -c1
                  Left(Between(c2, -c1, hom2))
                }
              } else {
                // nothing to do, incompatible
                Right((i1, i2))
              }
            }
          }
          case Between(bottom2, top2, hom2) => {
            if (hom1 == hom2) {
              // Compatible coeffs
              // bottom2 <= hom1 <= top2 & hom1 >= c1 =>  max(bottom2, c1) <= hom1 <= top2
              Left(Between(c1.max(bottom2), c1.min(top2), hom1))
            } else {
              if (hom1.map(_ * -1) == hom2) {
                // hom2 <= -c1
                // bottom2 <= hom2 <= top2 & -hom2 >= c1 =>  bottom2 <= hom2 <= min(-c1, top2)
                Left(Between(bottom2, -c1.min(top2), hom2))
              } else {
                // nothing to do, incompatible
                Right((i1, i2))
              }
            }
          }
        }
      case Eq(c1, hom1) => {
        i2 match {
          case Eq(c2, hom2) => {
            if (hom1 == hom2) {
              assert(c1 == c2)
              Left(i2)
            } else {
              if (hom1.map(_ * -1) == hom2) {
                assert(-c1 == c2)
                Left(i2)
              } else {
                // Incompatible
                Right((i1, i2))
              }
            }
          }
          case Ineq(c2, hom2) => {
            if (hom1 == hom2) {
              assert(i2.contains(c1))
              Left(i1)
            } else {
              if (hom1.map(_ * -1) == hom2) {
                // hom2 >= c2 & c1 = -hom2 => hom2 - -c1
                assert(i2.contains(-c1))
                Left(i1)
              } else {
                // Incompatible, nothing to do
                Right((i1, i2))
              }
            }
          }
          case Between(bottom2, top2, hom2) => {
            if (hom1 == hom2) {
              assert(i2.contains(c1))
              Left(i1)
            } else {
              if (hom1.map(_ * -1) == hom2) {
                assert(i2.contains(-c1))
                Left(i1)
              } else {
                // nothing to do, incompatible
                Right((i1, i2))
              }
            }
          }
        }
      }
      case Between(bottom1, top1, hom1) =>
        i2 match {
          case Eq(c2, hom2) => {
            if (hom1 == hom2) {
              assert(i1.contains(c2))
              Left(i1)
            } else {
              if (hom1.map(_ * -1) == hom2) {
                assert(i1.contains(-c2))
                Left(i1)
              } else {
                // nothing to do, incompatible
                Right((i1, i2))
              }
            }
          }
          case Ineq(c2, hom2) => {
            if (hom1 == hom2) {
              // bottom1 <= hom1 <= top1 & hom1 >= c2 => max(bottom1, c2) <= hom1 <= top1
              Left(Between(bottom1.max(c2), top1, hom2))
            } else {
              if (hom1.map(_ * -1) == hom2) {
                // bottom1 <= hom1 <= top1 & -hom1 >= c2 => hom1 <= - c2 => bottom1 <= hom1 <= min(-c2, top1)
                Left(Between(bottom1, top1.min(-c2), hom1))
              } else {
                // nothing to do, incompatible
                Right((i1, i2))
              }
            }
          }
          case Between(bottom2, top2, hom2) => {
            if (hom1 == hom2) {
              Left(Between(bottom1.max(bottom2), top1.min(top2), hom1)) // use tightest bound
            } else {
              if (hom1.map(_ * -1) == hom2) {
                //   bottom1 <= hom1 <= top1
                // & bottom2 <= -hom1 <= top2 => -top2 <= hom1 <= -bottom2
                // => max(bottom1, -top2) <= hom1 <= min(top1, -bottom2)
                Left(Between(bottom1.min(-top2), top1.max(-bottom2), hom1))
              } else {
                // nothing to do, incompatible
                Right((i1, i2))
              }
            }
          }
        }
    }
  }

  /**
    * Turns a set constraints into a set of inequalities, of the form
    *
    *  vi + ... + vj <= c
    *  vi + ... + vj = c
    *  c' <= vi + ... + <= c
    *
    *  for human readability; inequalities have support for pretty-printing
    */
  def constraintsToInequalities(constraints : Seq[LinearForm]) : Seq[Inequality] = {
    val cons : Seq[Inequality] = constraints.map((l) => Ineq(l.known, l.homcoeffs.map(_ * -1)))

    cons.foldRight(Seq[Inequality]())((i : Inequality,z : Seq[Inequality]) => {
      val foo = z.map((q : Inequality) => mergeIneq(q,i) match {
        case Left(a:Inequality) => Left(a)
        case Right((a,b)) => Right(q)
      })

      if(foo.map((x) => x match { case Left(_) => true case Right(_) => false }).filter((x) => x == true).length > 0)
        foo.map((x) => x match { case Left(y) => y case Right(y) => y })
      else
        i +: foo.map((x) => x match { case Left(y) => y case Right(y) => y })
    })
  }
}
