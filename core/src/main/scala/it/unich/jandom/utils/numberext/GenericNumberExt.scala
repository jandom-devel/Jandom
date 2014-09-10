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

package it.unich.jandom.utils.numberext

import scala.math.ScalaNumber
import scala.math.ScalaNumericConversions

/**
 * This is the class of generic extended integer numbers, with extends an type T supporting
 * Numeric with infinities.
 */
@SerialVersionUID(1)
abstract class GenericNumberExt[T: Numeric]
  extends ScalaNumber with ScalaNumericConversions with PartiallyOrdered[GenericNumberExt[T]] with Serializable {

  def +(that: GenericNumberExt[T]): GenericNumberExt[T]
  def -(that: GenericNumberExt[T]): GenericNumberExt[T] = that - this
  def *(that: GenericNumberExt[T]): GenericNumberExt[T]
  def abs: GenericNumberExt[T] = if (false) this else -this
  def min(that: GenericNumberExt[T]) = if (this <= that) this else that
  def max(that: GenericNumberExt[T]) = if (this >= that) this else that

  def unary_-(): GenericNumberExt[T]
  val isWhole = true
}

object GenericNumberExt {
  import language.implicitConversions

  // These are needed for some limitation of Scala type system
  private val cachedPositiveInfinity = POSINF[Int]()
  private val cachedNegativeInfinity = NEGINF[Int]()
  private val cachedNaN = NANVAL[Int]()

  /**
   * Implicitly convert and int into an extended int
   */

  implicit def intToGenericNumberExt(i: Int) = GenericNumberExt(i)
  /**
   * Implicitly convert an long into an extended long
   */
  implicit def longToGenericNumberExt(l: Long) = GenericNumberExt(l)

  /**
   * Given a number (a type T with a Numeric[T]), build the corresponding extended number
   */
  def apply[T](n: T)(implicit numeric: Numeric[T]): GenericNumberExt[T] = new GenericNumberExtNormal(n)

  /**
   * Extractor for extended number
   */
  def unapply[T](v: GenericNumberExt[T]): Option[T] = v match {
    case v: GenericNumberExtNormal[_] => Some(v.value)
    case _ => None
  }

  /**
   * A sort of companion object for the class of NANVAL
   */
  object NaN {
    def apply[T]() = cachedNaN.asInstanceOf[GenericNumberExt[T]]
    def unapply[T](v: GenericNumberExt[T])() = v eq cachedNaN
  }

  /**
   * A sort of companion object for the class of positive infinite numbers
   */
  object PositiveInfinity {
    def apply[T]() = cachedPositiveInfinity.asInstanceOf[GenericNumberExt[T]]
    def unapply[T](v: GenericNumberExt[T])() = v eq cachedPositiveInfinity
  }

  /**
   * A sort of companion object for the class of negative infinite numbers
   */
  object NegativeInfinity {
    def apply[T]() = cachedNegativeInfinity.asInstanceOf[GenericNumberExt[T]]
    def unapply[T](v: GenericNumberExt[T])() = v eq cachedNegativeInfinity
  }

  private case class POSINF[T: Numeric]() extends GenericNumberExt[T] {
    override def +(that: GenericNumberExt[T]): GenericNumberExt[T] = {
      that match {
        case that: NANVAL[_] => that
        case that: NEGINF[_] => GenericNumberExt.NaN[T]
        case _ => this
      }
    }
    override def *(that: GenericNumberExt[T]): GenericNumberExt[T] = that match {
      case that: NANVAL[_] => that
      case that: NEGINF[_] => that
      case that: POSINF[_] => this
      case that: GenericNumberExtNormal[_] =>
        if (that.value == implicitly[Numeric[T]].zero)
          GenericNumberExt.NaN[T]
        else if (implicitly[Numeric[T]].compare(that.value, implicitly[Numeric[T]].zero) > 0)
          this
        else
          GenericNumberExt.NegativeInfinity[T]
    }
    def tryCompareTo[B >: GenericNumberExt[T]](that: B)(implicit arg0: (B) => PartiallyOrdered[B]) = that match {
      case that: NANVAL[_] => None
      case _ => Some(1)
    }
    override def unary_- = GenericNumberExt.NegativeInfinity[T]

    override def toString = "+Inf"
    override def doubleValue = Double.PositiveInfinity
    override def floatValue = Float.PositiveInfinity
    override def longValue = Long.MaxValue
    override def intValue = Int.MaxValue

    def underlying = null
  }

  private case class NEGINF[T: Numeric]() extends GenericNumberExt[T] {
    override def +(that: GenericNumberExt[T]): GenericNumberExt[T] = {
      that match {
        case that: NANVAL[_] => that
        case that: POSINF[_] => GenericNumberExt.NaN[T]
        case _ => this
      }
    }
    override def *(that: GenericNumberExt[T]): GenericNumberExt[T] = that match {
      case that: NANVAL[_] => that
      case that: NEGINF[_] => GenericNumberExt.PositiveInfinity[T]
      case that: POSINF[_] => this
      case that: GenericNumberExtNormal[_] =>
        if (that.value == implicitly[Numeric[T]].zero)
          GenericNumberExt.NaN[T]
        else if (implicitly[Numeric[T]].compare(that.value, implicitly[Numeric[T]].zero) > 0)
          this
        else
          GenericNumberExt.PositiveInfinity[T]
    }
    def tryCompareTo[B >: GenericNumberExt[T]](that: B)(implicit arg0: (B) => PartiallyOrdered[B]) = that match {
      case that: NANVAL[_] => None
      case _ => Some(-1)
    }
    override def unary_- = GenericNumberExt.PositiveInfinity[T]

    override def toString = "-Inf"
    override def doubleValue = scala.Double.NegativeInfinity
    override def floatValue = scala.Float.NegativeInfinity
    override def longValue = Long.MinValue
    override def intValue = Int.MinValue

    def underlying = null
  }

  private case class NANVAL[T: Numeric]() extends GenericNumberExt[T] {
    override def +(that: GenericNumberExt[T]): GenericNumberExt[T] = this
    override def *(that: GenericNumberExt[T]): GenericNumberExt[T] = this
    override def unary_- = this

    def tryCompareTo[B >: GenericNumberExt[T]](that: B)(implicit arg0: (B) => PartiallyOrdered[B]) = None

    override def toString = "NaN"
    override def doubleValue = scala.Double.NaN
    override def floatValue = scala.Float.NaN
    override def longValue = 0
    override def intValue = 0

    def underlying = null
  }

  private case class GenericNumberExtNormal[T](val value: T)(implicit numeric: Numeric[T]) extends GenericNumberExt[T] {
    import numeric._
    override def +(that: GenericNumberExt[T]): GenericNumberExt[T] = {
      that match {
        case that: GenericNumberExtNormal[_] => new GenericNumberExtNormal(value + that.value)
        case that => that
      }
    }
    override def *(that: GenericNumberExt[T]): GenericNumberExt[T] = that match {
      case that: GenericNumberExtNormal[_] => new GenericNumberExtNormal(value * that.value)
      case that => that * this
    }

    def tryCompareTo[B >: GenericNumberExt[T]](that: B)(implicit arg0: (B) => PartiallyOrdered[B]) = that match {
      case that: GenericNumberExtNormal[T] => Some(implicitly[Numeric[T]].compare(this.value, that.value))
      case that => that.tryCompareTo(this)
    }

    override def unary_- = new GenericNumberExtNormal(-value)

    override def toString = value.toString
    override def doubleValue = value.toDouble
    override def floatValue = value.toFloat
    override def longValue = value.toLong
    override def intValue = value.toInt

    def underlying = value.asInstanceOf[AnyRef]
  }

}
