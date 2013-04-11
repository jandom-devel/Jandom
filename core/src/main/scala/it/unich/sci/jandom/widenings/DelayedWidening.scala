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

package it.unich.sci.jandom.widenings

import it.unich.sci.jandom.domains.NumericalProperty

/**
 * Delayed widening.
 * @param widening the original widening
 * @param delay the number of delayed steps 
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class DelayedWidening (private val widening: Widening, private var delay: Int) extends Widening { 
  require(delay>=0)
  
  def apply[Property <: NumericalProperty[Property]] (current: Property, next: Property) = {    
    if (delay>0) {
      delay -= 1
      current union next
    } else 
      widening(current, next)
  }
}
