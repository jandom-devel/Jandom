/**
 * This is the class for extended GenericNumbers (GenericNumbers with infinities)
 *
 * Copyright 2011 Gianluca Amato
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

package it.unich.sci.jandom.numberextensions

@SerialVersionUID(1)
sealed abstract class GenericNumberExt[T] extends NumberExt with Serializable {
  type Extension = GenericNumberExt[T]
  type Base = T

  def +(that: GenericNumberExt[T]): GenericNumberExt[T]
  def unary_-(): GenericNumberExt[T]

  def unary_+(): GenericNumberExt[T] = this
  def -(that: GenericNumberExt[T]): GenericNumberExt[T] = this + (-that)
}

object GenericNumberExt {
  // These are needed for some limitation of Scala type system
  private val cachedPositiveInfinity = POSINF()
  private val cachedNegativeInfinity = NEGINF()
  private val cachedNaN = NAN()

  implicit def intToGenericNumberExt(i: Int) = GenericNumberExt(i)
  implicit def longToGenericNumberExt(l: Long) = GenericNumberExt(l)

  def apply[T](n: T)(implicit numeric: Numeric[T]): GenericNumberExt[T] = new GenericNumberExtNormal(n)
  def unapply[T](v: GenericNumberExt[T]): Option[T] = v match {
    case v: GenericNumberExtNormal[_] => Some(v.value)
    case _ => None
  }

  object NaN {
    def apply[T]() = cachedNaN.asInstanceOf[GenericNumberExt[T]]
    def unapply[T](v: GenericNumberExt[T])() = v eq cachedNaN
  }

  object PositiveInfinity {
    def apply[T]() = cachedPositiveInfinity.asInstanceOf[GenericNumberExt[T]]
    def unapply[T](v: GenericNumberExt[T])() = v eq cachedPositiveInfinity
  }

  object NegativeInfinity {
    def apply[T]() = cachedNegativeInfinity.asInstanceOf[GenericNumberExt[T]]
    def unapply[T](v: GenericNumberExt[T])() = v eq cachedNegativeInfinity
  }

  private case class POSINF[T]() extends GenericNumberExt[T] {
    override def +(that: GenericNumberExt[T]): GenericNumberExt[T] = {
      that match {
        case that: NAN[_] => that
        case that: NEGINF[_] => GenericNumberExt.NaN[T]
        case _ => this
      }
    }
    override def unary_- = GenericNumberExt.NegativeInfinity[T]

    override def toString = "+Inf"
    override def doubleValue = scala.Double.PositiveInfinity
    override def floatValue = scala.Float.PositiveInfinity
    override def longValue = Long.MaxValue
    override def intValue = Int.MaxValue
  }

  private case class NEGINF[T]() extends GenericNumberExt[T] {
    override def +(that: GenericNumberExt[T]): GenericNumberExt[T] = {
      that match {
        case that: NAN[_] => that
        case that: POSINF[_] => GenericNumberExt.NaN[T]
        case _ => this
      }
    }
    override def unary_- = GenericNumberExt.PositiveInfinity[T]

    override def toString = "-Inf"
    override def doubleValue = scala.Double.NegativeInfinity
    override def floatValue = scala.Float.NegativeInfinity
    override def longValue = Long.MinValue
    override def intValue = Int.MinValue
  }

  private case class NAN[T]() extends GenericNumberExt[T] {
    override def +(that: GenericNumberExt[T]): GenericNumberExt[T] = this
    override def unary_- = this

    override def toString = "NaN"
    override def doubleValue = scala.Double.NaN
    override def floatValue = scala.Float.NaN
    override def longValue = 0
    override def intValue = 0
  }

  private case class GenericNumberExtNormal[T](val value: T)(implicit numeric: Numeric[T]) extends GenericNumberExt[T] {
    import numeric._
    override def +(that: GenericNumberExt[T]): GenericNumberExt[T] = {
      that match {
        case that: GenericNumberExtNormal[_] => new GenericNumberExtNormal(value + that.value)
        case that => that
      }
    }
    override def unary_- = new GenericNumberExtNormal(-value)

    override def toString = value.toString
    override def doubleValue = value.toDouble
    override def floatValue = value.toFloat
    override def longValue = value.toLong
    override def intValue = value.toInt
  }

}

