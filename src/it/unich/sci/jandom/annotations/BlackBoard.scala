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
package it.unich.sci.jandom.annotations

import scala.collection.mutable.HashMap
import it.unich.sci.jandom.targets.Target

/**
 * A blackboard for putting type-safe annotations for a specific target.
 * @author Gianluca Amato <g.amato@unich.it>
 *
 */
class BlackBoard[Tgt <: Target](t: Tgt) (implicit maker: PerProgramPointAnnotationBuilder[Tgt])  {
  private val myhash = new HashMap[AnnotationType, Annotation[Tgt,_]] 
      
  def update[Ann <: AnnotationType]( kind: Ann, v: Annotation[Tgt,Ann] ) {
    myhash(kind)=v
  }   
  
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
