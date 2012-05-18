/**
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
 *
 * (c) 2012 Gianluca Amato
 */

package it.unich.sci.jandom
package narrowings

import domains.NumericalProperty
import targets.Target
import ppfactories.ConstantFactory

/**
 * This is the trait for narrowings, operators used to accelerate fixpoint computations.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
trait Narrowing {
  /**
   * @param current the property at the current iteration.
   * @param next the property at the next iteration. This IS assumed to be smaller than current.
   * @result the result of the narrowing.
   */
  def apply[Property <: NumericalProperty[Property]](current: Property, next: Property) : Property
}

/**
 * The companion object for Narrowing. Contains implicit definition to transform automatically
 * a narrowing into a constant "per program point" factory.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
object Narrowing {
  /** 
   * Transforms a narrowing in a constant narrowing factory
   * @return a constant narrowing factory for the parameter narrowing.
   */
  implicit def toNarrowingFactory(narrowing: Narrowing) = ConstantFactory(narrowing)
}
