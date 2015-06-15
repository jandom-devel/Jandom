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
 * The `Box` object defines some factories for building boxes, i.e. functions of the
 * type (V,V)=>V for some V which are abstractions of widenings, narrowings, etc..
 */
object Box {
  /**
   * A box which always returns its right component (new contribution).
   */
  def right[V]: Box[V] = { (x: V, y: V) => y }

  /**
   * A box which always returns its left component (original value).
   */
  def left[V]: Box[V] = { (x: V, y: V) => x }

  /**
   * A box which coincides with the `first` box for `delay` calls,
   * and then coincides with the `second` box. This may be used to implement
   * delayed widening and narrowing.
   */
  def cascade[V](first: Box[V], delay: Int, second: Box[V]): Box[V] = {
    require(delay >= 0)
    var steps = delay

    { (x: V, y: V) =>
      if (steps > 0) {
        steps -= 1
        first(x, y)
      } else {
        second(x, y)
      }
    }
  }
}
