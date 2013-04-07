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

package it.unich.sci.jandom.targets.slil

import it.unich.sci.jandom.domains.NumericalProperty
import it.unich.sci.jandom.targets.Annotation

/**
 * The class for tags, i.e. fake statements only used to mark program points. The 
 * analysis at this point is reported trough the `tag` array in the `Parameters`
 * object.
 * @param tag an integer which is the index of this tag in the `tag` array
 */
case class TagStmt(tag: Int) extends SLILStmt {
  import AnalysisPhase._

  override def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[ProgramPoint,params.Property]): params.Property = {
    params.tag(tag) = input
    input
  }

  override def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint,U], level: Int, ppspec: PrettyPrinterSpec) =
    ppspec.indent(level) + "tag(" + tag + ")\n"
}
