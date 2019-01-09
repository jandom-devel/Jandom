/**
  * Copyright 2013, 2018 Gianluca Amato
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

package it.unich.jandom.targets.slil

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.{Annotation, Environment, lts}
import it.unich.jandom.ui.output.OutputBuilder

/**
  * The class for tags, i.e. fake statements only used to mark program points. The
  * analysis at this point is reported trough the `tag` array in the `Parameters`
  * object.
  *
  * @param tag an integer which is the index of this tag in the `tag` array
  */
class TagStmt(val tag: Int) extends SLILStmt {

  import AnalysisPhase._

  def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase,
                                      ann: Annotation[ProgramPoint, params.Property]): params.Property = {
    params.tag(tag) = input
    input
  }

  def outputAnnotation[T <: NumericalProperty[_]](ann: Annotation[(Tgt, Any), T], ob: OutputBuilder, env: Environment): Unit = {
    ob ++= s"tag($tag)"
  }

  def syntacticallyEquals(that: SLILStmt): Boolean = that match {
    case that: TagStmt => tag == that.tag
    case _ => false
  }

  val numvars = 0

  def toLTS(prev: lts.Location, next: lts.Location): (Map[ProgramPoint, lts.Location], Seq[lts.Transition]) = {
    (Map.empty, Seq(lts.Transition(this.toString, prev, next, Seq.empty, Seq.empty)))
  }

  override def toString = s"tag($tag)"
}

object TagStmt {
  def apply(tag: Int): TagStmt = new TagStmt(tag)
}
