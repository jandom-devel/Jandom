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

package it.unich.jandom.ui

import it.unich.jandom.targets.parameters.WideningScope._

/**
 * The ParameterEnumeration for WideningScope.
 */
object WideningScopes extends ParameterEnumeration[Value] {
  val name = "Widening Scope"
  val description = "The Widening scope"
  val values = Seq(
      ParameterValue(Output, "Output", "The standard widening, which is applied to the output edge"),
      ParameterValue(BackEdges, "Back Edges", "The widening is applied at the input back edges"),
      ParameterValue(Random, "Localized", "The widening is applied like in Random (a variant of Back Edge)")
      )
  val default = values(2)
}
