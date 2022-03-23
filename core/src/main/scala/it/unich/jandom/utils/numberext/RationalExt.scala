/**
 * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>, Marco Rubino  <marco.rubino@unich.it>
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

package it.unich.jandom.utils.numberext

import java.math.RoundingMode

import scala.language.implicitConversions
import scala.math.ScalaNumber
import scala.math.ScalaNumericConversions

import spire.math.Number
import spire.math.Rational
import spire.math.SafeLong
import scala.runtime.RichDouble

/**
 * RationalExt is the class of rational numbers extended with infinite values and NaN's. It
 * depends on the Rational class in Spire for implementing the operations on standard rational
 * values. As it is customary, a NaN is treated in a special way: all binary operations involving
 * a NaN returns NaN, and all binary tests fail when one of the two operands is a NaN.
 * @param value a rational value. This is meaningful only when the `special` field is `NORMAL`
 * @param special one of the values in the RationalExt enumeration. They are `NORMAL`, `POSINF`,
 * `NEGINF` and `NAN`, with the obvious meaning.
 */
class RationalExt(val value: Rational, private val special: RationalExt.SpecialValue.SpecialValue) extends ScalaNumber with ScalaNumericConversions with IField[RationalExt] {

  import RationalExt.SpecialValue._

  def +(that: RationalExt): RationalExt = (special, that.special) match {
    case (NORMAL, NORMAL) => new RationalExt(this.value + that.value, NORMAL)
    case (NORMAL, POSINF) | (POSINF, NORMAL) | (POSINF, POSINF) => RationalExt.PositiveInfinity
    case (NORMAL, NEGINF) | (NEGINF, NORMAL) | (NEGINF, NEGINF) => RationalExt.NegativeInfinity
    case _ => RationalExt.NaN
  }

  def -(that: RationalExt): RationalExt = (special, that.special) match {
    case (NORMAL, NORMAL) => new RationalExt(this.value - that.value, NORMAL)
    case (NORMAL, NEGINF) | (POSINF, NORMAL) | (POSINF, NEGINF) => RationalExt.PositiveInfinity
    case (NORMAL, POSINF) | (NEGINF, NORMAL) | (NEGINF, POSINF) => RationalExt.NegativeInfinity
    case _ => RationalExt.NaN
  }

  def *(that: RationalExt): RationalExt = (special, that.special) match {
    case (NORMAL, NORMAL) => new RationalExt(this.value * that.value, NORMAL)
    case (NORMAL, POSINF) => value.signum match {
      case 1 => RationalExt.PositiveInfinity
      case -1 => RationalExt.NegativeInfinity
      case 0 => RationalExt.NaN
    }
    case (NORMAL, NEGINF) => value.signum match {
      case 1 => RationalExt.NegativeInfinity
      case -1 => RationalExt.PositiveInfinity
      case 0 => RationalExt.NaN
    }
    case (POSINF, NORMAL) | (NEGINF, NORMAL) => that * this
    case (POSINF, POSINF) | (NEGINF, NEGINF) => RationalExt.PositiveInfinity
    case (NEGINF, POSINF) | (POSINF, NEGINF) => RationalExt.NegativeInfinity
    case _ => RationalExt.NaN
  }

  def /(that: RationalExt): RationalExt = (special, that.special) match {
    case (NORMAL, NORMAL) => {
      if (!that.value.isZero)
        new RationalExt(value / that.value, NORMAL)
      else value.signum match {
        case -1 => RationalExt.NegativeInfinity
        case 0 => RationalExt.NaN
        case 1 => RationalExt.PositiveInfinity
      }
    }
    case (NORMAL, POSINF) | (NORMAL, NEGINF) => RationalExt.zero
    case (POSINF, NORMAL) => if (that.value.signum >= 0) RationalExt.PositiveInfinity else RationalExt.NegativeInfinity
    case (NEGINF, NORMAL) => if (that.value.signum >= 0) RationalExt.NegativeInfinity else RationalExt.PositiveInfinity
    case _ => RationalExt.NaN
  }

  def unary_+ = this

  def unary_- = special match {
    case NORMAL => new RationalExt(- value, NORMAL)
    case POSINF => RationalExt.NegativeInfinity
    case NEGINF => RationalExt.PositiveInfinity
    case _ => RationalExt.NaN
  }

  def abs: RationalExt = special match {
    case NORMAL => new RationalExt(value.abs, NORMAL)
    case POSINF | NEGINF => RationalExt.PositiveInfinity
    case _ => RationalExt.NaN
  }

  /**
   * Returns true if the value is 0
   */
  def isZero = special == NORMAL && value.isZero

  /**
   * Returns true if the value is +∞.
   */
  def isPosInfinity = special == POSINF

  /**
   * Returns true if the number is -∞.
   */
  def isNegInfinity = special == NEGINF

  /**
   * Returns true if the number is infinite.
   */
  def isInfinity = isPosInfinity || isNegInfinity

  override def toString = special match {
    case NORMAL => value.toString()
    case POSINF => "Infinity"
    case NEGINF => "-Infinity"
    case NAN => "NaN"
  }

  def doubleValue = special match {
    case POSINF => scala.Double.PositiveInfinity
    case NEGINF => scala.Double.NegativeInfinity
    case NAN => scala.Double.NaN
    case NORMAL => value.doubleValue
  }

  def floatValue = special match {
    case POSINF => scala.Float.PositiveInfinity
    case NEGINF => scala.Float.NegativeInfinity
    case NAN => scala.Float.NaN
    case NORMAL => value.floatValue
  }

  def longValue = special match {
    case POSINF => Long.MaxValue
    case NEGINF => Long.MinValue
    case NAN => throw new IllegalArgumentException("cannot convert NaN to Long")
    case NORMAL => value.longValue
  }

  def intValue = special match {
    case POSINF => Int.MaxValue
    case NEGINF => Int.MinValue
    case NAN => throw new IllegalArgumentException("cannot convert NaN to Int")
    case NORMAL => value.intValue
  }

  def max(that: RationalExt): RationalExt = (special, that.special) match {
    case (NORMAL, NORMAL) => new RationalExt(this.value max that.value, NORMAL)
    case (NAN, _) | (_, NAN) => RationalExt.NaN
    case (NEGINF, _) => that
    case (_, NEGINF) => this
    case _ => RationalExt.PositiveInfinity
  }

  def min(that: RationalExt): RationalExt = (special, that.special) match {
    case (NORMAL, NORMAL) => new RationalExt(this.value min that.value, NORMAL)
    case (NAN, _) | (_, NAN) => RationalExt.NaN
    case (POSINF, _) => that
    case (_, POSINF) => this
    case _ => RationalExt.NegativeInfinity
  }

  /**
   * The compare method for `RationalExt` is somewhat inconsistent with the rest of the
   * comparison operator since it should linearly order the NaN value.
   */
  def compare(that: RationalExt) = (this.special, that.special) match {
    case (NORMAL, NORMAL) => this.value compare that.value
    case _ => this.special compare that.special
  }

  def >(that: RationalExt) = (special, that.special) match {
    case (NORMAL, NORMAL) => value > that.value
    case (NAN, _) | (_, NAN) => false
    case (POSINF, NORMAL) | (POSINF, NEGINF) | (NORMAL, NEGINF) => true
    case _ => false
  }

  def >=(that: RationalExt) = (special, that.special) match {
    case (NORMAL, NORMAL) => value >= that.value
    case (NAN, _) | (_, NAN) => false
    case (POSINF, _) | (NORMAL, NORMAL) | (NORMAL, NEGINF) | (NEGINF, NEGINF) => true
    case _ => false
  }

  def <(that: RationalExt): Boolean = that > this

  def <=(that: RationalExt): Boolean = that >= this

  def !=(that: RationalExt): Boolean = (special, that.special) match {
    case (NORMAL, NORMAL) => value != that.value
    case (NAN, NAN) => true
    case _ => special != that.special
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: RationalExt => (special, that.special) match {
        case (NORMAL, NORMAL) => value == that.value
        case (POSINF, POSINF) | (NEGINF, NEGINF) => true
        case _ => false
      }
      case _ => if (special == NORMAL)
        value == that
      else
        false
    }

  def underlying = this

  def isWhole = value.isWhole && special == NORMAL

  /**
   * Returns a string representing the rational number. For finite rationals, a decimal representation
   * is produced, tuned by the `scale` and `mode` parameters.
   */
  def mkString(scale: Int, mode: RoundingMode) = special match {
    case NORMAL => value.toBigDecimal(scale, mode).toString
    case POSINF => "Infinity"
    case NEGINF => "-Infinity"
    case NAN => "NaN"
  }

  def _div_2 : RationalExt =
    if (isPosInfinity) this
    else if (isNegInfinity) this
    else (this / 2)

  def _x_2 : RationalExt =
    if (isPosInfinity) this
    else if (isNegInfinity) this
    else (this * 2)
}

object RationalExt extends StaticIField[RationalExt]{
  /**
   * The enumeration of the special values for the `RationalExt` class.
   */
  private object SpecialValue extends Enumeration {
    type SpecialValue = Value
    val NEGINF = Value(0)
    val NORMAL = Value(1)
    val POSINF = Value(2)
    val NAN = Value(3)
  }

  import SpecialValue._

  val one = new RationalExt(Rational.one, NORMAL)

  val minusOne = new RationalExt(-Rational.one, NORMAL)

  val zero = new RationalExt(Rational.zero, NORMAL)

  val PositiveInfinity = new RationalExt(Rational.zero, POSINF)

  val NegativeInfinity = new RationalExt(Rational.zero, NEGINF)

  val NaN = new RationalExt(Rational.zero, NAN)

  implicit def apply(x: Rational): RationalExt = new RationalExt(x, NORMAL)

  /**
   * Returns a rational corresponding to the string  `r`. Strings Infinity, -Infinity
   * and NaN are recognized for the corresponding spcial values.
   */
  def apply(r: String): RationalExt = r match {
    case "Infinity" => RationalExt.PositiveInfinity
    case "-Infinity" => RationalExt.NegativeInfinity
    case "NaN" => RationalExt.NaN
    case _ => apply(Rational(r))
  }

  def apply(x: Number): RationalExt = apply(Rational(x))

  def apply(n: SafeLong, d: SafeLong): RationalExt = apply(Rational(n, d))

  def apply(n: BigInt, d: BigInt): RationalExt = apply(Rational(n, d))

  def apply(n: Long, d: Long): RationalExt = apply(Rational(n, d))

  implicit def apply(x: Int): RationalExt = apply(Rational(x))

  implicit def apply(x: Long): RationalExt = apply(Rational(x))

  implicit def apply(x: SafeLong): RationalExt = apply(Rational(x))

  implicit def apply(x: BigInt): RationalExt = apply(Rational(x))

  implicit def apply(d: Double): RationalExt = {
    // We explicitly create a RichDouble since new implicits prevents automatic conversion
    val rd = new RichDouble(d)
    if (rd.isPosInfinity)
      RationalExt.PositiveInfinity
    else if (rd.isNegInfinity)
      RationalExt.NegativeInfinity
    else if (rd.isNaN)
      RationalExt.NaN
    else
      new RationalExt(Rational(d), NORMAL)
  }

  implicit def apply(x: Float): RationalExt = apply(x.toDouble)

  implicit def apply(x: BigDecimal): RationalExt = apply(Rational(x))

  /**
   * An instance of the `Fractional` type class for RationalExt.
   */
  implicit object RationalExtendedIsFractional extends Fractional[RationalExt] {
    def plus(x: RationalExt, y: RationalExt): RationalExt = x + y
    def minus(x: RationalExt, y: RationalExt): RationalExt = x - y
    def times(x: RationalExt, y: RationalExt): RationalExt = x * y
    def negate(x: RationalExt): RationalExt = -x
    def fromInt(x: Int): RationalExt = RationalExt(Rational(x))
    def toInt(x: RationalExt): Int = x.toInt
    def toLong(x: RationalExt): Long = x.toLong
    def toFloat(x: RationalExt): Float = x.toFloat
    def toDouble(x: RationalExt): Double = x.toDouble
    def div(x: RationalExt, y: RationalExt): RationalExt = x / y
    def compare(x: RationalExt, y: RationalExt) = x compare y

    /**
      * This is a fake implementation which always returns None
      * @todo write a real implementation
      */
    def parseString(str: String): Option[RationalExt] = None
  }
}
