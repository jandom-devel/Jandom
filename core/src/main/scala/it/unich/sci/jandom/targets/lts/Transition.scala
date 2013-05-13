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

package it.unich.sci.jandom.targets.lts

import it.unich.sci.jandom.domains.numerical.NumericalProperty
import it.unich.sci.jandom.targets.LinearAssignment
import it.unich.sci.jandom.targets.linearcondition.LinearCond

/**
 * The class for transitions.
 * @param name the name of the transition
 * @param start the source location of the transition
 * @param end the target location of the transition
 * @param guard the conditions which should hold for the transition to be active
 * @param assignments the assignments to apply when the transition is selected
 * @author Gianluca Amato <amato@sci.unich.it>
 */
case class Transition (val name: String, val start: Location, val end: Location, val guard: Seq[LinearCond], val assignments: Seq[LinearAssignment[_]]) {
  end += this
  
  override def toString = "transition "+name+" "+start.name+" -> "+end.name + " with Guard( " + 
	guard.mkString(", ") + " )\n" +
	assignments.mkString(start="  ", sep="\n  ", end="")+";"

  def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = {
    val filtered = (input /: guard) { (current, cond) => cond.analyze(current) }
	(filtered /: assignments)  { (current, assgn) => assgn.analyze(current) }
  }     
}
