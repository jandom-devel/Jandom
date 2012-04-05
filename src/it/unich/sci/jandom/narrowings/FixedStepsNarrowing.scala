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
package it.unich.sci.jandom.narrowings

import it.unich.sci.jandom.domains.NumericalProperty
import it.unich.sci.jandom.targets.Target
import it.unich.sci.jandom.annotations._

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
  
class FixedStepsNarrowing[Property <: NumericalProperty[Property]] (private val narrowing: Narrowing[Property], private val steps: Int) extends Narrowing[Property] { 
  require(steps>0)
  
  def apply[Tgt <: Target] (current: Property, next: Property, bb: BlackBoard[Tgt], pp: Tgt#ProgramPoint) = {
    val i = bb(FixedStepNarrowingAnnotation)(pp)
    if (i < steps) {
      bb(FixedStepNarrowingAnnotation)(pp) = i+1
      next
    } else 
      narrowing(current, next, bb, pp)
  } 
}

/**
 * This declares a new annotation type for the current index of fixed step narrowing
 */
object FixedStepNarrowingAnnotation extends AnnotationType {
  type T = Int
  val defaultValue = 0
}