/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.targets.parameters

/**
 * This objects determines the scope for widenings. The available alternatives are:
 * - Output: standard application of widening at the exit of join nodes
 * - BackEdges: widening is applied at the entrance of join nodes, but only on back edges
 * - Random: localized widening as described in the paper submitted to SAS 2013.
 * At the moment, this is only supported by the SLIL target.
 */

object WideningScope extends Enumeration {  
  type WideningScope = Value
  val Output = Value
  val BackEdges = Value
  val Random = Value  
}
