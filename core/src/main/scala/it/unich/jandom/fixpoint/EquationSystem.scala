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
 * This is the trait for a generic equation system.
 */
trait EquationSystem {
  /**
   * The type of the unknowns for this equation system.
   */
  type Unknown

  /**
   * The type of values for this equation system.
   */
  type Value

  /**
   * An assignment of values to unknowns, which is a candidate solution.
   */
  type Assignment = Unknown => Value

  /**
   * A way to combine the value in the previous iteration with the value in the
   * new iteration.
   */
  type Box = (Value, Value) => Value

  /**
   * The actual equation system.
   */
  def apply(rho: Assignment): Assignment
}
