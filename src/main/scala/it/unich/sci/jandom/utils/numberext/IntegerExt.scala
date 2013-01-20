/**
 * This class implements extended integers (integers with infinities)
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

package it.unich.sci.jandom
package utils.numberext

import NumberExt.SpecialValues._

/**
 * This is the class of extended integers.
 * @param val the integer value of the numbers, when special is NORMAL
 * @param special the special value of the number, or NORMAL if it is an integer
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
@SerialVersionUID(1)
class IntegerExt(val value: Int, val special: Value) extends NumberExt with Serializable {    

  type Extension = IntegerExt
  
  override def +(that: IntegerExt): IntegerExt = (special,that.special) match {
    case (NORMAL,NORMAL) => new IntegerExt(value+that.value,NORMAL)
    case (NORMAL,_) => that
    case (_,NORMAL) => this
    case (POSINF, NEGINF) => IntegerExt.NaN
    case (NEGINF, POSINF) => IntegerExt.NaN
    case _ => that
  }  
  override def -(that: IntegerExt):IntegerExt = this + (-that)
  override def unary_+ = this
  override def unary_- = special match {
    case NORMAL => new IntegerExt(-value, special)
    case POSINF => IntegerExt.NegativeInfinity
    case NEGINF => IntegerExt.PositiveInfinity
    case NAN => this
  }    
    
  override def toString = special match {
    case NORMAL => value.toString
    case POSINF => "+Inf"
    case NEGINF => "-Inf"
    case NAN => "NaN"  
  }         
  override def doubleValue = special match {
    case POSINF => scala.Double.PositiveInfinity
    case NEGINF => scala.Double.NegativeInfinity
    case NAN => scala.Double.NaN
    case NORMAL => value.doubleValue
  }
  override def floatValue = special match {
    case POSINF => scala.Float.PositiveInfinity
    case NEGINF => scala.Float.NegativeInfinity
    case NAN => scala.Float.NaN
    case NORMAL => value.floatValue
  }  
  override def longValue = special match {
    case POSINF => Long.MaxValue
    case NEGINF => Long.MinValue
    case NAN => throw new IllegalArgumentException("cannot convert NaN to Long")
    case NORMAL => value.longValue
  }
  override def intValue = special match {
    case POSINF => Int.MaxValue
    case NEGINF => Int.MinValue
    case NAN => throw new IllegalArgumentException("cannot convert NaN to Long")
    case NORMAL => value.intValue
  }    
  
  override def equals(other: Any): Boolean = {
    other match {
      case other: IntegerExt => this.value == other.value && this.special == other.special
      case _ => false
    }        
  }
  override def hashCode(): Int = {
    41*(41 + this.value) + this.special.id 
  }
}

/**
 * The companion object for extended integers
 */
object IntegerExt {
  import language.implicitConversions

  /** 
   * An implicit conversion of integer to extended integers.
   */
  implicit def intToIntegerExt(i: Int) = apply(i)   
  
  /**
   * The positive infinity for extended integers 
   */
  val PositiveInfinity = new IntegerExt(0, POSINF)
  
  /**
   * The negative infinity for extended integers 
   */
  val NegativeInfinity = new IntegerExt(0, NEGINF)
  
  /**
   * The NAN for extended integers 
   */
  val NaN = new IntegerExt(0, NAN)
  
  /** 
   * Build an extended integers
   * @param n an integer
   * @return the corresponding extended integer
   */
  
  def apply(n: Int) = new IntegerExt(n, NORMAL)
  
  /**
   * The extractor for extended integers
   */
  def unapply(v: IntegerExt) = v.special match {
    case NORMAL => Some(v.value)
    case _ => None    
  }
}
