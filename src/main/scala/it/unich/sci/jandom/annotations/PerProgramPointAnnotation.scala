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
package annotations

import targets.Target

/**
 * A PerProgramPointAnnotation is an target-defined implementation of a map
 * from program points to values of a given annotation type.
 * @tparam Tgt the target of this annotation
 * @tparam Ann the annotation type
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
abstract class PerProgramPointAnnotation[Tgt <: Target, Ann <: AnnotationType] extends scala.collection.mutable.Iterable[(Tgt#ProgramPoint,Ann#T)]  {
  /** 
   * Method to access the annotation of a given program point
   */
  def apply(pp: Tgt#ProgramPoint): Ann#T
  
  /**
   * Method to update the annotation of a given program point
   */
  def update(pp: Tgt#ProgramPoint, v: Ann#T)
  
  override def toString = (map { case (l,v) =>  l + " --> " + v }).mkString(start="",sep="\n",end="\n")
}
