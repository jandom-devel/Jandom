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

package it.unich.jandom

import it.unich.jandom.utils.IterableFunction

/**
 * The fixpoint package contains everything which is related to defining and solving systems
 * of equations. This package object defines some type aliases which are used in the API.
 */
package object fixpoint {
  /**
   * An assignment for an equation system is a map from unknowns to values. 
   */
  type Assignment[U,V] = U => V
  
  /**
   * A partial assignment for an equation system is an iterable function from unknowns to
   * values.
   */
  type PartialAssignment[U,V] = IterableFunction[U,V]
  
  /**
   * A Box is a way to combine two values into a new one. It is an alias for the type
   * (V,V) => V, where the first parameter is the old value and the second value is the
   * new contribution. Both widening and narrowing are examples of boxes.
   * @tparam V the type of the values to combine.
   */
  type Box[V] = (V, V) => V
}
