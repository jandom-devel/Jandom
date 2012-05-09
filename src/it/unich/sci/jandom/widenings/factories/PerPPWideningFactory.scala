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
import annotations._
import scala.collection.mutable.Map

/**
 * A widening factory which uses different widenings for different program points, but reuses widenings
 * within the same program point.
 * @tparam Tgt the type of target for the widening factory
 * @param wideningFactory the factory to build new widenings when needed
 * @param ann a per-pp annotation used to stored the generated widenings 
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class PerPPWideningFactory[Tgt <: Target] (private val wideningFactory: WideningFactory[Tgt], 
                                           private val ann: PerProgramPointAnnotation[Tgt,PerPPWideningAnnotation.type]) extends WideningFactory[Tgt] {     
  def apply(pp: Tgt#WideningPoint) = {    
    ann(pp) match {
      case Some(w) => w
      case None => { 
        val w = wideningFactory(pp)
        ann(pp) = Some(w) 
        w 
      }
    }
  }
}

/** 
 * An annotation type used to store the widenings for the different program points.
 */
private object PerPPWideningAnnotation extends AnnotationType {
  type T = Option[Widening]
  val defaultValue = None
}

/**
 * The companion object for per program points widening factories
 */
object PerPPWideningFactory {
  /**
   * Build a Per PP widening factory knowing a PerProgramPointAnnotation
   * @tparam Tgt the type of target for the widening factory
   * @param wideningFactory the factory to build new widenings when needed
   * @param ann a per-pp annotation used to stored the generated widenings
   * @return a per-pp factory
   */
  def apply[Tgt <: Target](wideningFactory: WideningFactory[Tgt], ann: PerProgramPointAnnotation[Tgt,PerPPWideningAnnotation.type]) = 
    new PerPPWideningFactory[Tgt](wideningFactory, ann)
  
  /**
   * Build a Per PP widening factory using the standard PerProgramPointAnnotation for a given target
   * @tparam Tgt the type of target for the widening factory
   * @param wideningFactory the factory to build new widenings when needed
   * @param tgt the target 
   * @param annBuilder the program point annotation builder for the given target
   * @return a per-pp factory
   */
  def apply[Tgt <: Target](wideningFactory: WideningFactory[Tgt], tgt: Tgt) (implicit annBuilder: PerProgramPointAnnotationBuilder[Tgt]) = 
    new PerPPWideningFactory[Tgt](wideningFactory, annBuilder(tgt, PerPPWideningAnnotation))
}
