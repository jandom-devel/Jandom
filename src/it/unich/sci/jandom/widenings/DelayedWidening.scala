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

import it.unich.sci.jandom.domains.NumericalProperty
import it.unich.sci.jandom.targets.{Annotations,AnnotationType}

/**
 * This build a delayed widening.
 * @tparam Property: the property the widening acts upon
 * @param delay the delay of the widening
 * @param widening the original widening
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class DelayedWidening[Property <: NumericalProperty[Property]] (private val widening: Widening[Property], private val delay: Int) extends Widening[Property] {  
  def apply[ProgramPoint](current: Property, next: Property, ann: Annotations[ProgramPoint], pp: ProgramPoint) = {
    // get the index of the delayed widening at the current program point
    val i = ann.get(pp, DelayedWideningAnnotation) match {
      case None => 0
      case Some(j) => j
    }
    if (i < delay) {      
      // increment index
      ann(pp, DelayedWideningAnnotation) = i+1
      current union next
    } else 
      widening(current, next, ann, pp)
  }
}

/**
 * This declares a new annotation type for the current index of delayed widening
 */
object DelayedWideningAnnotation extends AnnotationType[Int]
