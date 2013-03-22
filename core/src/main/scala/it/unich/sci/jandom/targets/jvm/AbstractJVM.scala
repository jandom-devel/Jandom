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

package it.unich.sci.jandom.targets.jvm

import scala.collection.mutable.{ ArrayBuffer, ArrayStack, Buffer }

import it.unich.sci.jandom.domains.{ NumericalDomain, NumericalProperty }

/**
 * This is an abstract JVM.
 */
class AbstractJVM[Property <: NumericalProperty[Property]](
  val frame: Array[Int], val stack: ArrayStack[Int], var property: Property) {

  def iconst(v: Int) {   
    property = property.addDimension
    property = property.constantAssignment(property.dimension - 1, v)  
    stack.push(property.dimension - 1)
  }
  
  def istore(i: Int) {
    frame(i) = stack.pop
  }
  
  override def toString = 
    "Frame: "+frame.mkString("<", ",", ">") + " Stack: " + stack.mkString("<", ",", ">") + " Property: " + property    
}

object AbstractJVM {
  def apply(dom: NumericalDomain, maxFrame: Int) = new AbstractJVM[dom.Property](Array.fill(maxFrame)(-1), ArrayStack[Int](), dom.full(0))
}
