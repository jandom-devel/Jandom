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

import domains.{NumericalProperty, NumericalPropertyAnnotation}

import annotations._
import scala.collection.mutable.ArrayBuffer

/**
 * A mock target only used for test suites. It is private for the jandom package.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
private[jandom] class MockTarget extends Target {
  type Tgt = MockTarget
  type ProgramPoint = Int
  def size = 10
  def analyze[Property <: NumericalProperty[Property]] (params: Parameters[Property]): Annotation = { 
    MockTarget.MockProgramPointAnnotationBuilder(this,NumericalPropertyAnnotation) 
  }
}

/** 
 * The companion object for the MockTarget. It defines the AnnotationBuilder for program point annotations.
 * @author Gianluca Amato <amato@sci.unich.it> 
 */
private[jandom] object MockTarget {
  implicit object MockProgramPointAnnotationBuilder extends PerProgramPointAnnotationBuilder[MockTarget] {
	 def apply[Ann <: AnnotationType](t: MockTarget, ann: Ann): PerProgramPointAnnotation[MockTarget,Ann] = 
	   new PerProgramPointAnnotation[MockTarget,Ann]{
	     val a = ArrayBuffer.fill[Ann#T](t.size)(ann.defaultValue)
	     def apply(pp: MockTarget#ProgramPoint) = a(pp)
	     def update(pp: MockTarget#ProgramPoint, v: Ann#T) { a(pp) = v }
	     def iterator = new Iterator[(MockTarget#ProgramPoint,Ann#T)] {
	       var index: Int = -1
	       def hasNext = index < a.size -1
	       def next = { index +=1 ; (index,a(index)) }
	     }
	 }
  } 
}
