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
package targets

import domains.NumericalProperty
import annotations.BlackBoard

/**
 * The abstract class for targets.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
abstract class Target {
  /**
   * The type for program points of the given target
   */
  type ProgramPoint
  
  /**
   * The type of the given target
   */
  type Tgt <: Target
  
  /**
   * Returns the size of the target as the number of program points.
   * @return the size of the target as the number of program points
   */
  def size: Int
  
  /**
   * Perform a static analysis over the target.
   * @tparam Property the type of the property we want to analyze
   * @param param the parameters which drive the analyzer
   * @param bb the blackboard where it is possible to put annotation during the analysis
   */  
  def analyze[Property <: NumericalProperty[Property]] (params: Parameters[Property, Tgt], bb: BlackBoard[Tgt])
}
