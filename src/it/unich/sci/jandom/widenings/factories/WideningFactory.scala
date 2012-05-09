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
package widenings
package factories

import targets.Target

/**
 * This is a factory for widenings and is a parameter of the analysis. The analyzer calls the
 * factory at each program point, and the factory decides whether to create a new widening or
 * reuse an existing one.
 * @tparam Tgt the target for the widening factory
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
abstract class WideningFactory[-Tgt <: Target] {
  /**
   * Returns a widening.
   * @param pp the program point this widening is supposed to be used
   * @return the widening to use at program point pp
   */
  def apply (pp: Tgt#WideningPoint): Widening
}
