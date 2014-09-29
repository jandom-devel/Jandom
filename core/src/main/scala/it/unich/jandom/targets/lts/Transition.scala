/**
 * Copyright 2013, 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.targets.lts

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.NumericAssignment
import it.unich.jandom.targets.linearcondition.LinearCond

/**
 * The class for transitions.
 * @param name the name of the transition
 * @param start the source location of the transition
 * @param end the target location of the transition
 * @param guard the conditions which should hold for the transition to be active
 * @param assignments the assignments to apply when the transition is selected
 * @author Gianluca Amato <gamato@unich.it>
 */
case class Transition(val name: String, val start: Location, val end: Location, val guard: Seq[LinearCond], val assignments: Seq[NumericAssignment]) {
  end += this

  def mkString(vars: Seq[String]) = {
     "transition " + name + " " + start.name + " -> " + end.name + " with Guard( " +
    (guard map { _.mkString(vars) }).mkString(", ") + " )\n" +      
    (assignments map { _.mkString(vars) }).mkString(start = "  ", sep = "\n  ", end = "") + ";"
  }

  override def toString = mkString(Stream.from(0).map { "v" + _ })

  def analyze[Property <: NumericalProperty[Property]](input: Property): Property = {
    val filtered = (input /: guard) { (current, cond) => cond.analyze(current) }
    (filtered /: assignments) { (current, assgn) => assgn.analyze(current) }
  }
}
