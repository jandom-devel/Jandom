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

package it.unich.sci.jandom
package annotations

import targets.Target

/**
 * This is the abstract base class for target-defined implementations of a map
 * from program points to values of a given annotation type. Similarly to
 * [[it.unich.sci.jandom.annotations.Annotation]], they are parametric w.r.t. a
 * [[it.unich.sci.jandom.targets.Target]] and an 
 * [[it.unich.sci.jandom.annotations.AnnotationType]].
 * @tparam Tgt the target type for this annotation.
 * @tparam Ann the annotation type class for this annotation.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
abstract class PerProgramPointAnnotation[Tgt <: Target, Ann <: AnnotationType] extends scala.collection.mutable.Iterable[(Tgt#ProgramPoint,Ann#T)]  {
  /** 
   * Returns the annotation at a given program point. If no annotation exists, a new one is created
   * with the default value of `Ann`.
   */
  def apply(pp: Tgt#ProgramPoint): Ann#T
  
  /**
   * Updates the annotation at a given program point.
   */
  def update(pp: Tgt#ProgramPoint, v: Ann#T)
  
  override def toString = (map { case (l,v) =>  l + " --> " + v }).mkString(start="",sep="\n",end="\n")
}
