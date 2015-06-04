/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.fixpoint

/**
 * A Box is a way to combine two values into a new one. It is essentially a function
 * (V,V) => V where the first parameter is the old value and the second value is the 
 * new contribution. Both widening and narrowing are examples of boxes. 
 * @tparam V the type of the values to combine.
 */
abstract class Box[V] extends Function2[V, V, V]

/**
 * The companion object for boxes essentially contains many useful factories.
 */
object Box {

  // Unfortunately, there is no way to use the function literal notation for boxes.
  
  /**
   * Returns a box whose behavior is described by a function.
   * @param boxf the function which combines values.
   */
  def apply[V](boxf: (V,V) => V) = new functionalBox(boxf) 
  
  /**
   * A box whose behavior is described by a function.
   * @param boxf the function which combines values.
   */
  implicit class functionalBox[V](boxf: (V, V) => V) extends Box[V] {
    def apply(x: V, y: V) = boxf(x, y)
  }

  /**
   * Returns a box which always returns its right component.
   */
  def right[V] = new Box[V] {
    def apply(x: V, y: V) = y
  } 
  
  /**
   * Returns a box which always returns its left component.
   */
  def left[V] = new Box[V] {
    def apply(x: V, y: V) = x
  }

  /**
   * Returns a box which coincides with the `first` box for `delay` calls,
   * and then coincides with the `second` box. This may be used to implement
   * delayed widening and narrowing.
   */
  def cascade[V](first: Box[V], delay: Int, second: Box[V]) = new Box[V] {    
    require(delay >= 0)    
    var steps = delay

    def apply(x: V, y: V) = {
      if (steps > 0) {
        steps -= 1
        first(x, y)
      } else {
        second(x, y)
      }
    }
  }
}
