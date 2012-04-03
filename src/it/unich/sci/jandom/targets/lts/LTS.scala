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
import it.unich.sci.jandom.targets.{Environment,Parameters,Annotations,Target}

/**
 * The main class for Linear Transition Systems.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class LTS (val locations: List[Location], val transitions: List[Transition], val environment: Environment) extends Target {    
  type ProgramPoint = Int
    
  override def toString = locations.mkString("\n") + "\n" + transitions.mkString("\n")
  
  def analyze[Property <: NumericalProperty[Property]](domain: NumericalDomain[Property], params: Parameters[Property], ann: Annotations[ProgramPoint]) {   
    var current, next : List[Property] = Nil    
    next = for (loc <- locations) 
      yield  (domain.full(environment.size) /: loc.conditions) { (prop, cond) => cond.analyze(prop) }
    while (current != next) {      
      current = next
      next =  for ((loc, propold) <- locations zip current) yield {
        val propnew = for (t <- loc.transitions) yield t.analyze(propold)
        val unionednew = propnew.fold( domain.empty(environment.size) ) ( _ union _ )
        params.widening(propold, unionednew, ann, loc.id)
      }      
    } 
    current = Nil
    while (current != next) {
      current = next
      next =  for ((loc, propold) <- locations zip current) yield {
        val propnew = for (t <- loc.transitions) yield t.analyze(propold)
        val unionednew = propnew.fold( domain.empty(environment.size) ) ( _ union _ )
        params.narrowing(propold, unionednew, ann, loc.id)    
      }      
    }
   current.zipWithIndex.foreach { case (prop, pp) => ann(pp, NumericalPropertyAnnotation)=prop }     
  }      
}
