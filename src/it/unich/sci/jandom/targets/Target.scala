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
package it.unich.sci.jandom.targets

import it.unich.sci.jandom.domains._
import it.unich.sci.jandom.annotations._

/**
 * The abstract class for targets
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
abstract class Target {
  type ProgramPoint
  type Tgt <: Target
  
  /**
   * Returns the size of the target as the number of program points.
   */
  def size: Int
  
  /**
   * Takes a domain and some parameters and perform the analysis.
   */
  def analyze[Property <: NumericalProperty[Property]](params: Parameters[Property], bb: BlackBoard[Tgt])
}
