/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.ui

import it.unich.jandom.domains.numerical.ppl.PPLInitializer
import it.unich.jandom.domains.numerical.ppl.PPLDomainMacro

import parma_polyhedra_library._

/**
 * This object register a list of PPL based numerical domains [[it.unich.jandom.ui.NumericalDomains]].
 * The registered domains are implemented through macros.
 * @author Gianluca Amato <gamato@unich.it>
 */

private[ui] object PPLMacroUIInitializer {
  if (PPLInitializer.isSuccessful) NumericalDomains.values ++= Seq(
    ParameterValue(PPLDomainMacro[Double_Box], "PPL Double_Box", "PPL based implementation of boxes over double (using macros)."),
    ParameterValue(PPLDomainMacro[Octagonal_Shape_double], "PPL Octagonal_Shape_double", "PPL based implementation of Octagon over double (using macros)."),
    ParameterValue(PPLDomainMacro[C_Polyhedron], "PPL C_Polyhedron", "PPL based implementation of closed polyhedra (using macros)."))    
}
