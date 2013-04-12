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
/**
 * The ParameterEnumeration for numerical domains.
 */

package it.unich.sci.jandom.ui

import it.unich.sci.jandom.domains.BoxDouble
import it.unich.sci.jandom.domains.NumericalDomain
import it.unich.sci.jandom.domains.PPLDomain
import it.unich.sci.jandom.domains.Parallelotope
import it.unich.sci.jandom.domains.PPLPropertyMacros

import parma_polyhedra_library.C_Polyhedron
import parma_polyhedra_library.Double_Box
import parma_polyhedra_library.Octagonal_Shape_double

/**
 * The ParameterEnumeration for numerical domains.
 */
object NumericalDomains extends ParameterEnumeration[NumericalDomain] {  
  val name = "Domain"
  val description = "The numerical domain to use for the analysis."
  val values: Seq[ParameterValue[NumericalDomain]] = Seq(
    ParameterValue(BoxDouble, "BoxDouble", "This is a native Scala implementation of boxes. It is not safe " +
      "and should not be used."),
    ParameterValue(Parallelotope, "Parallelotope", "This is a native Scala implementation of parallelotopes. It is " +
      "not safe and should not be used."),
    ParameterValue(PPLPropertyMacros[Double_Box], "PPL Double_Box", "PPL based implementation of boxes over double (using macros)."),
    ParameterValue(PPLPropertyMacros[Octagonal_Shape_double], "PPL Octagonal_Shape_double", "PPL based implementation of Octagon over double (using macros)."),
    ParameterValue(PPLPropertyMacros[C_Polyhedron], "PPL C_Polyhedron", "PPL based implementation of closed polyhedra (using macros).")
    )
  val default = values.last.value
}
