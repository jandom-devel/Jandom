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
 * This is a region in an LTS. A region is a named pair made of an optional location and a numeric condition.
 * It may be used to keep various information such as starting conditions, bad nodes, etc...
 * At the moment, it is not used for the analysis.
 * @author Gianluca Amato <gamato@unich.it>
 */
case class Region(val name: String, val state: Option[Location], val condition: NumericCondition) {

  def mkString(vars: Seq[String]) = if (state.isEmpty)
    s"region ${name} with ( ${condition.mkString(vars)} );"
  else
    s"region ${name} on state = ${state.get.name} with ( ${condition.mkString(vars)} );"

  def mkString: String = mkString(Stream.from(0).map { "v" + _ })

  override def toString = name
}
