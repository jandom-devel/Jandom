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

import targets.linearcondition.LinearCond


/**
 * A class for locations (i.e. nodes in a linear transition system)
 * @param name name of the location
 * @param id numerical id of the location
 * @conditions linear conditions which should hold at the location
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class Location (val name:String, val id: LTS#ProgramPoint, val conditions: Seq[LinearCond]) {   
  /**
   * The set of incoming transitions 
   */
  var incoming : List[Transition] = Nil
  
  /**
   * Add a transition to the set of incoming transitions.
   * @param t the transition to add
   * @return the transition t
   */
  def += (t: Transition): Transition = { 
    incoming = t :: incoming 
    t 
  }
  
  /**
   * Returns the iterable of incoming transitions.
   * @return the incoming transitions
   */
  def transitions = incoming.toIterable
    
  override def toString = 
    "location "+name+" with (\n"+ 
       conditions.mkString(start="  ", sep="\n  ", end="\n") + 
    ");" 
}
