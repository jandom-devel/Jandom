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
package it.unich.sci.jandom.targets.lts

import it.unich.sci.jandom.domains._
import it.unich.sci.jandom.targets.{Environment,Parameters,Target}
import it.unich.sci.jandom.annotations._
import scala.collection.mutable.ArrayBuffer
/**
 * The main class for Linear Transition Systems.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class LTS (val locations: List[Location], val transitions: List[Transition], val environment: Environment) extends Target {    
  
  type ProgramPoint = Int
  type Tgt = LTS
  
  override def toString = locations.mkString("\n") + "\n" + transitions.mkString("\n")
  
  def size = locations.size
  
  def analyze[Property <: NumericalProperty[Property]] (params: Parameters[Property, Tgt], bb: BlackBoard[LTS]) {    
    var current, next : List[Property] = Nil    
    next = for (loc <- locations) 
      yield  (params.domain.full(environment.size) /: loc.conditions) { (prop, cond) => cond.analyze(prop) }
           
    while (current != next) {      
      current = next
      next =  for ((loc, propold) <- locations zip current) yield {
        val propnew = for (t <- loc.transitions) yield t.analyze(propold)
        val unionednew = propnew.fold( params.domain.empty(environment.size) ) ( _ union _ )
        params.widening[LTS](propold, unionednew, bb, loc.id)
      }      
    } 
    
    current = Nil    
    while (current != next) {
      current = next
      next =  for ((loc, propold) <- locations zip current) yield {
        val propnew = for (t <- loc.transitions) yield t.analyze(propold)
        val unionednew = propnew.fold( params.domain.empty(environment.size) ) ( _ union _ )
        params.narrowing[LTS](propold, unionednew, bb, loc.id)    
      }      
    }
    val annotation = bb(NumericalPropertyAnnotation)  
    current.zipWithIndex.foreach { case (prop, pp) => annotation(pp)=prop }         
  }      
}

object LTS {
  implicit object LTSProgramPointAnnotationBuilder extends PerProgramPointAnnotationBuilder[LTS] {
	 def apply[Ann <: AnnotationType](t: LTS, ann: Ann): PerProgramPointAnnotation[LTS,Ann] = new PerProgramPointAnnotation[LTS,Ann]{
	   val a = ArrayBuffer.fill[Ann#T](t.size)(ann.defaultValue)
	   def apply(pp: LTS#ProgramPoint) = a(pp)
	   def update(pp: LTS#ProgramPoint, v: Ann#T) { a(pp) = v }
	 }
  } 
}
