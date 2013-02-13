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
package targets.slil

import annotations.PerProgramPointAnnotation
import it.unich.sci.jandom.domains.NumericalProperty
import it.unich.sci.jandom.domains.AbstractProperty

/**
 * The class for the empty statement.
 */
case object NopStmt extends SLILStmt {
  override def mkString[U <: AbstractProperty] (ann: Annotation[U], level: Int, ppspec: PrettyPrinterSpec): String =
    ppspec.indent(level) + "<no-op>"
}
