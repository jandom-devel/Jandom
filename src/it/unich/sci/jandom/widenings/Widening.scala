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
package it.unich.sci.jandom.widenings

import it.unich.sci.jandom.targets.Annotations

/**
 * This is the abstract widening, an operator used to accelerate fixpoint computations.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
abstract class Widening[Property] {
  /**
   * @param current the property at the current iteration
   * @param next the property at the next iteration
   * @result the result of widening 
   */
  def apply[ProgramPoint](current: Property, next: Property, ann: Annotations[ProgramPoint], pp: ProgramPoint) : Property
}
