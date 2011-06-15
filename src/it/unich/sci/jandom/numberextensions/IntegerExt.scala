/**
 * This class implements integer with infinities
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

import NumberExt.SpecialValues._

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class IntegerExt(val value: Int, val special: Value) extends NumberExt {    
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

object IntegerExt {
  implicit def intToIntegerExt(i: Int) = apply(i)   
  
  val PositiveInfinity = new IntegerExt(0, POSINF)
  val NegativeInfinity = new IntegerExt(0, NEGINF)
  val NaN = new IntegerExt(0, NAN)
  
  def apply(n: Int) = new IntegerExt(n, NORMAL)
  def unapply(v: IntegerExt) = v.special match {
    case NORMAL => Some(v.value)
    case _ => None    
  }
}
