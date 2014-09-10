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

/**
 * This is the base class for numeric extensions.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
abstract class NumberExt extends java.lang.Number with Serializable {
  import NumberExt.SpecialValues._
  
  /**
   * This is the real type of the NumberExt.
   */
  protected type Extension <: NumberExt

  def +(that: Extension): Extension
  def -(that: Extension): Extension
  def unary_-(): Extension 
  def unary_+(): Extension
}

/** 
 * The companion object of NumberExt keeps the enumeration of all the special values
 * (i.e. infinities and NAN), used internally by our implementations.
 */
object NumberExt {  
  /**
   * The enumeration of special values.
   */
  private[numberext] object SpecialValues extends Enumeration {
    val POSINF = Value
    val NEGINF = Value
    val NAN = Value
    val NORMAL = Value
  }  
}
