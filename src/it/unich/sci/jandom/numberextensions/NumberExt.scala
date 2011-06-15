/**
 * This is the ancestor of all the number extensions
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

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
abstract class NumberExt extends java.lang.Number with Serializable {
  import NumberExt.SpecialValues._
  
  type Extension <: NumberExt

  def +(that: Extension): Extension
  def -(that: Extension): Extension
  def unary_-(): Extension 
  def unary_+(): Extension
}

object NumberExt {
  object SpecialValues extends Enumeration {
    val POSINF = Value
    val NEGINF = Value
    val NAN = Value
    val NORMAL = Value
  }  
}
