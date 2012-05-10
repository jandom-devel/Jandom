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

/**
 * Delayed narrowing. It performs delayes normal descending steps, and then start applying the
 * narrowing given as an argument.
 * @param narrowing the original narrowing
 * @param delay the number of delayed steps 
 * @author Gianluca Amato <amato@sci.unich.it>
 */
  
class DelayedNarrowing(private val narrowing: Narrowing, private var delay: Int) extends Narrowing { 
  require (delay>=0)
  
  def apply[Property <: NumericalProperty[Property]]  (current: Property, next: Property) = {      
    if (delay>0) {
      delay -= 1
      next
    } else 
      narrowing(current, next)
  } 
}
