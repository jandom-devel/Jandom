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

package it.unich.sci.jandom
package ui

/**
 * This objects determines the scope for widenings. The available alternatives are:
 * - Output: standard application of widening at the exit of join nodes
 * - BackEdges: widening is applied at the entrance of join nodes, but only on back edges
 * - Random: the scope used on Random. Widening is applied at the exit of join nodes, but join is only
 *           applied once.
 * At the moment, this is only supported by the SLIL target.
 */
object WideningScope extends ParameterEnumeration {  
  type WideningScope = Value
  val name = "Widening Scope"
  val shortName = "widening"
  val description = "The Widening scope"
  val Output = Value("Output", "The standard widening, which is applied to the output edge")
  val BackEdges = Value("Back Edges", "The widening is applied at the input back edges")
  val Random = Value("Random", "The widening is applied like in Random (a variant of Back Edge)")
  val default = Random
}
