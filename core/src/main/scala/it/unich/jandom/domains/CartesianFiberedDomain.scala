/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains

/**
 * This is the trait for a cartesian fibered domain. The fibers of this domain are
 * identified by finite sequence of elements.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait CartesianFiberedDomain extends AbstractDomain {
  /**
   * The type of components of a fiber identifier.
   */
  type FiberComponent

  type Fiber = Seq[FiberComponent]

  type Property <: CartesianFiberedProperty[FiberComponent, Property]

  /**
   * Returns the top element of the given fiber.
   */
  def top(f: Fiber): Property

  /**
   * Returns the bottom element of the given fiber.
   */
  def bottom(f: Fiber): Property
}
