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

import it.unich.jandom.targets.NumericCondition

/**
 * A class for locations (i.e. nodes in a linear transition system)
 * @param name name of the location
 * @param conditions linear conditions which should hold at the location
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
case class Location(val name: String, val conditions: Seq[NumericCondition]) {
  /**
   * The set of incoming transitions. Used internally by the analyzer.
   */
  private[lts] var incoming = List[Transition]()

  /**
   * The set of outgoing transitions. Used internally by the analyzer.
   */
  private[lts] var outgoing = List[Transition]()

  /**
   * A numeric id for the location. Used internally by the analyzer. The value
   * -1 means it has never been assigned a valid id.
   */
  private[lts] var id: Int = -1

  /**
   * The position of this location in the depth-first order. The value -1
   * means that the dfo has not been computed yet.
   */
  private[lts] var dfo: Int = -1

  /**
   * Used internally by several visit algorithms.
   */
  private[lts] var visited: Boolean = false

  def mkString(vars: Seq[String]) =
    "location " + name + " with (\n" +
      (conditions map { _.mkString(vars) }).mkString(start = "  ", sep = "\n  ", end = "\n") +
      ");"

  def mkString: String = mkString(Stream.from(0).map { "v" + _ })

  override def toString = name
}
