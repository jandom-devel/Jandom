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
import it.unich.jandom.targets._
import it.unich.jandom.ui.output.OutputBuilder

/**
  * The class for the assignment statement "variable := linearForm".
  *
  * @param variable the variable we are assign a value to
  * @param expr     the numeric expression of the assignment
  */
class AssignStmt(val variable: Int, val expr: NumericExpression) extends SLILStmt {

  import AnalysisPhase._

  def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[ProgramPoint, params.Property]): params.Property =
    expr.assignTo(variable)(input)

  def outputAnnotation[T <: NumericalProperty[_]](ann: Annotation[ProgramPoint, T], ob: OutputBuilder, env: Environment): Unit = {
    ob ++= s"${env(variable)} = ${expr.mkString(env.variables)}"
  }

  def syntacticallyEquals(that: SLILStmt): Boolean = that match {
    case that: AssignStmt => variable == that.variable && expr == that.expr
    case _: SLILStmt => false
  }

  val numvars: Int = expr.dimension max (variable + 1)

  def toLTS(prev: lts.Location, next: lts.Location): (Map[ProgramPoint, lts.Location], Seq[lts.Transition]) = {
    (Map.empty, Seq(lts.Transition(this.toString, prev, next, Seq.empty, NumericAssignment(variable, expr))))
  }

  override def toString = s"v$variable = ${expr.toString}"
}

object AssignStmt {
  def apply(variable: Int, expr: NumericExpression): AssignStmt = new AssignStmt(variable, expr)
}