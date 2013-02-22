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

package it.unich.sci.jandom
package ppfactories

import targets.Target

/**
 * This is the trait for "per program point" factories, i.e. functions
 * which build objects parameterized by widening points.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
trait PPFactory[-Tgt <: Target, +Val] extends Function1[Tgt#WideningPoint, Val]

object PPFactory {
  /**
   * A "per program point" implicit factory which always returns the same value
   * @tparam T the type of the object built by the factory
   * @param obj the object returned by the factory
   * @author Gianluca Amato <amato@sci.unich.it>
   */
  implicit class ConstantFactory[T](private val obj: T) extends PPFactory[Target, T] {
    def apply(pp: Target#WideningPoint) = obj
  }
}
