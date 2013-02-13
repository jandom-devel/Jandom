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
import scala.collection.mutable.HashMap

/**
 * A blackboard for putting type-safe annotations for a specific target. 
 * @author Gianluca Amato <g.amato@unich.it>
 *
 */
class BlackBoard[Tgt <: Target](t: Tgt) (implicit maker: PerProgramPointAnnotationBuilder[Tgt])  {
  /**
   * An hash from annotation types to annotations. The way it is handled by
   * Blackboard make it typesafe.
   */
  private val myhash = new scala.collection.mutable.HashMap[AnnotationType, Annotation[Tgt,_]] 
      
  /**
   * Update the annotation in the blackboard corresponding to an annotation type.
   */
  def update[Ann <: AnnotationType]( kind: Ann, v: Annotation[Tgt,Ann] ) {
    myhash(kind)=v
  }   
  
  /**
   * Given an annotation type, returns the corresponding annotation in the blackboard.
   */
  def apply[Ann <: AnnotationType] ( kind: Ann ) :  Annotation[Tgt,Ann] = {
    myhash.get(kind) match {
      case None => {
        val default = new Annotation[Tgt,Ann](t, kind)
        myhash(kind) = default
        default
      }
      case Some(v) => v.asInstanceOf[Annotation[Tgt,Ann]]
    }
  }
    
  override def toString = {    
    myhash map { case (key, value) => key + "\n" + value } mkString("--\n")    
  }
}
