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
  * This is a region in an LTS. A region is a named pair made of an optional location and a numeric condition.
  * It may be used to keep various information such as starting conditions, bad nodes, etc...
  * At the moment, a region with name init is used to specify an initial state and numeric condition.
  *
  * @param name      name of the region
  * @param state     an optional state corresponding to this region
  * @param condition a numeric condition corresponding to this region
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */
case class Region(val name: String, val state: Option[Location], val condition: NumericCondition) {

  /**
    * Returns true if `that` is syntactically equal to `this`.
    */
  def syntacticallyEquals(that: Region): Boolean =
    name == that.name &&
      (state zip that.state).forall(l => l._1.syntacticallyEquals(l._2)) &&
      condition == that.condition

  /**
    * Returns a string representation of a region with given variable names.
    */
  def mkString(vars: Seq[String]): String = if (state.isEmpty)
    s"region $name with ( ${condition.mkString(vars)} );"
  else
    s"region $name on state = ${state.get.name} with ( ${condition.mkString(vars)} );"

  /**
    * Returns a string representation of a region with synthetic variable names.
    */
  def mkString: String = mkString(LazyList.from(0).map("v" + _))

  override def toString: String = name
}
