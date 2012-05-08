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
package targets.lts

import domains.{NumericalProperty,NumericalPropertyAnnotation}
import targets.{Environment,Parameters,Target}
import widenings.Widening
import annotations._

import scala.collection.mutable.ArrayBuffer

/**
 * The class for the target of Linear Transition Systems.
 * @param locations the locations which makes the LTS
 * @param transitions the transitions which makes the LTS
 * @param env the environment of the LTS
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class LTS (val locations: Seq[Location], val transitions: Seq[Transition], val env: Environment) extends Target {    
  
  type ProgramPoint = Int
  type Tgt = LTS
  
  override def toString = locations.mkString("\n") + "\n" + transitions.mkString("\n")
  
  def size = locations.size
  
  def analyze[Property <: NumericalProperty[Property]] (params: Parameters[Property, Tgt], bb: BlackBoard[LTS]) {    
    var current, next, base : Seq[Property] = Nil
    val widenings = (0 to locations.size-1) map { params.wideningFactory(_) } 

    next = for (loc <- locations) 
      yield  (params.domain.full(env.size) /: loc.conditions) { (prop, cond) => cond.analyze(prop) }   
    
    while (current != next) {      
      current = next      
      next = for ((loc, propold) <- locations zip current) yield {
        val propnew = for (t <- loc.transitions) yield t.analyze(current(t.start.id))
        val unionednew = propnew.fold( params.domain.empty(env.size) ) ( _ union _ )
        widenings(loc.id)(propold, unionednew)
      }      
    } 
    
    current = Nil    
    while (current != next) {
      current = next
      next =  for ((loc, propold) <- locations zip current) yield {
        val propnew = for (t <- loc.transitions) yield t.analyze(current(t.start.id))
        val unionednew = propnew.fold( params.domain.empty(env.size) ) ( _ union _ )
        params.narrowing[LTS](propold, unionednew, bb, loc.id)    
      }      
    }
   
    val annotation = bb(NumericalPropertyAnnotation) 
    current.zipWithIndex.foreach { case (prop, pp) => annotation(pp)=prop }         
  }      
}

/** 
 * The companion object for LTS. It defines the AnnotationBuilder for program point annotations. 
 */
object LTS {
  /**
   * The annotation builder for program point annotations in LTS's
   */
  implicit object LTSProgramPointAnnotationBuilder extends PerProgramPointAnnotationBuilder[LTS] {
	 def apply[Ann <: AnnotationType](t: LTS, ann: Ann): PerProgramPointAnnotation[LTS,Ann] = 
	   new PerProgramPointAnnotation[LTS,Ann]{
	     val a = ArrayBuffer.fill[Ann#T](t.size)(ann.defaultValue)
	     def apply(pp: LTS#ProgramPoint) = a(pp)
	     def update(pp: LTS#ProgramPoint, v: Ann#T) { a(pp) = v }
	     def iterator = new Iterator[(LTS#ProgramPoint,Ann#T)] {
	       var index: Int = -1
	       def hasNext = index < a.size -1
	       def next = { index +=1 ; (index,a(index)) }
	     }
	 }
  } 
}
