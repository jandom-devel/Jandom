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

import it.unich.jandom.targets.linearcondition.LinearCond

/**
 * A class for locations (i.e. nodes in a linear transition system)
 * @param name name of the location
 * @param conditions linear conditions which should hold at the location
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
case class Location(val name: String, val conditions: Seq[LinearCond]) {
  /**
   * The set of incoming transitions.  Used internally by the analyzer
   */
  private[this] var inc: List[Transition] = Nil

  /**
   * A numeric id for the location. Used internally by the analyzer. The value
   * -1 means it has never been assigned a valid id.
   */
  private[lts] var id: Int = -1

  /**
   * Returns the incoming transitions. Used internally by the analyzer
   * @return the incoming transitions
   */
  private[lts] def incomings = inc

  /**
   * Add a transition to the set of incoming transitions.  Used internally by the analyzer
   * @param t the transition to add
   * @return the transition t
   */
  private[lts] def +=(t: Transition): Transition = {
    inc = t :: inc
    t
  }

  def mkString(vars: Seq[String]) =
    "location " + name + " with (\n" +
      (conditions map { _.mkString(vars) }).mkString(start = "  ", sep = "\n  ", end = "\n") +
      ");"

  override def toString = mkString(Stream.from(0).map { "v" + _ })

}
