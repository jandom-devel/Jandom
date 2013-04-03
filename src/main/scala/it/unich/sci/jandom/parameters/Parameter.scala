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

package it.unich.sci.jandom.parameters

/** 
 * Every parameter which may be used in the analyzer should mix the
 * trait Parameter.
 */
trait Parameter[V] {
  /**
   * The name of the parameter, to appear in the UI
   */
  val name: String
  
  /**
   * A description of the parameter to be used, for example, in tooltips
   */
  val description: String  
  
  /**
   * A list of enabled values for this parameter
   */
  def enabledValues: Seq[V with ParameterValue]
  
  /**
   * Default value of the parameter
   */
  val default: V with ParameterValue
}
