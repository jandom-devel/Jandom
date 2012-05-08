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
import annotations.{AnnotationType,PerProgramPointAnnotationBuilder}

/**
 * A widening factory which builds a different widening for each program point
 * @tparam Tgt the target for the widening factory
 * @param wideningFactory the factory to build the widenings at each program point
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class PerPPWideningFactory[Tgt <: Target] (private val wideningFactory: WideningFactory[Tgt], tgt: Tgt)
										   (implicit annBuilder: PerProgramPointAnnotationBuilder[Tgt]) extends WideningFactory[Tgt] {

  object PerPPWideningAnnotation extends AnnotationType {
    type T = Widening
	val defaultValue = null
  }
  
  private val bb = annBuilder(tgt, PerPPWideningAnnotation)
  
  def apply(pp: Tgt#ProgramPoint) =  {
    val w = bb(pp)
    if (w!=null) 
      w 
    else {
      val neww = wideningFactory(pp)
      bb(pp) = neww 
      neww
    }
  }
}
