/**
  * Copyright 2013, 2014, 2018 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
  * JANDOM is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * JANDOM is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
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
  *
  * @param name       name of the location
  * @param conditions linear conditions which should hold at the location
  * @author Gianluca Amato <gianluca.amato@unich.it>
  *
  */
class Location(val name: String, val conditions: Seq[NumericCondition]) {
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

  /**
    * Returns true if `that` is syntactically equal to `this`.
    */
  def syntacticallyEquals(that: Location): Boolean =
    name == that.name && conditions == that.conditions

  /**
    * Returns a string representation of an annotation with given variable names.
    */
  def mkString(vars: Seq[String]): String =
    "location " + name + " with (\n" +
      (conditions map (_.mkString(vars))).mkString(start = "  ", sep = "\n  ", end = "\n") + ");"

  /**
    * Returns a string representation of an annotation with synthetic variable names.
    */
  def mkString: String = mkString(Stream.from(0).map("v" + _))

  override def toString: String = name
}

object Location {
  def apply(name: String, conditions: Seq[NumericCondition]): Location = new Location(name, conditions)
}
