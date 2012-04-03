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
package it.unich.sci.jandom.targets

import scala.collection.mutable.HashMap

/**
 * Annotations for programs.
 * @author Gianluca Amato <g.amato@unich.it>
 *
 */

class Annotations (t: Target) {
  private val myhash = new HashMap[Tuple2[t.ProgramPoint, AnnotationType[_]], Any] 
  
  def get[T](pp: t.ProgramPoint, kind: AnnotationType[T] ): Option[T] = 
    myhash.get((pp,kind)) match {
      case None => None
      case v: Some[T] => v
      case _ => throw new IllegalAccessException
  }
  
  def update[T](pp: t.ProgramPoint, kind: AnnotationType[T], v: T ) {
    myhash((pp,kind))=v
  }
}
