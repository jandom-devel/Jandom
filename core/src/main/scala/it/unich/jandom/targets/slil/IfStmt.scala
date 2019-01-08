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
import it.unich.jandom.targets.{Annotation, NumericCondition, lts}

/**
  * The class for an if/then/else statement.
  *
  * @param condition   the guard of the statement
  * @param then_branch the statement to execute when the guard is true
  * @param else_branch the statement to execute when the guard is false
  */
case class IfStmt(condition: NumericCondition, then_branch: SLILStmt, else_branch: SLILStmt) extends SLILStmt {

  import AnalysisPhase._

  def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase,
                                      ann: Annotation[ProgramPoint, params.Property]): params.Property = {
    val thenStart = condition.analyze(input)
    val elseStart = condition.opposite.analyze(input)
    val thenEnd = then_branch.analyzeStmt(params)(thenStart, phase, ann)
    val elseEnd = else_branch.analyzeStmt(params)(elseStart, phase, ann)
    thenEnd union elseEnd
  }

  def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint, U], ppspec: SLILPrinterSpec,
                                          row: Int, level: Int): String = {
    val spaces = ppspec.indent(level)
    val then_string = then_branch.mkString(ann, ppspec, row + 1, level + 1)
    val else_string = else_branch.mkString(ann, ppspec, row + 2 + then_string.count(_ == '\n'), level + 1)
    spaces + "if (" + condition.mkString(ppspec.env.names) + ") {\n" +
      then_string +
      spaces + "} else {\n" +
      else_string +
      spaces + "}\n"
  }

  val numvars: Int = condition.dimension max then_branch.numvars max else_branch.numvars

  def toLTS(prev: lts.Location, next: lts.Location): (Map[ProgramPoint, lts.Location], Seq[lts.Transition]) = {
    val pp1 = lts.Location((this, 1).toString, Seq.empty)
    val pp2 = lts.Location((this, 2).toString, Seq.empty)
    val pp1end = lts.Location((this, 3).toString, Seq.empty)
    val pp2end = lts.Location((this, 4).toString, Seq.empty)
    val t1 = lts.Transition((this, 'thenBranch).toString, prev, pp1, Seq(condition), Seq.empty)
    val t2 = lts.Transition((this, 'elseBranch).toString, prev, pp2, Seq(condition.opposite), Seq.empty)
    val t3 = lts.Transition((this, 'thenExit).toString, pp1end, next, Seq.empty, Seq.empty)
    val t4 = lts.Transition((this, 'elseExit).toString, pp2end, next, Seq.empty, Seq.empty)
    val tt1 = then_branch.toLTS(pp1, pp1end)
    val tt2 = else_branch.toLTS(pp2, pp2end)
    (tt1._1 ++ tt2._1, tt1._2 ++ tt2._2 :+ t1 :+ t2 :+ t3 :+ t4)
  }

  /*
  // Optimized alternative definition of wire.

  def toLTS(prev: lts.Location, next: lts.Location): (Map[ProgramPoint, lts.Location], Seq[lts.Transition]) = {
    val pp1 = lts.Location((this, 1).toString, Seq.empty)
    val pp2 = lts.Location((this, 2).toString, Seq.empty)
    val t1 = lts.Transition((this, 'thenBranch).toString, prev, pp1, Seq(condition), Seq.empty)
    val t2 = lts.Transition((this, 'elseBranch).toString, prev, pp2, Seq(condition.opposite), Seq.empty)
    val tt1 = then_branch.toLTS(pp1, next)
    val tt2 = else_branch.toLTS(pp2, next)
    (tt1._1 ++ tt2._1 ++ Map((this, 1) -> pp1, (this, 2) -> pp2), tt1._2 ++ tt2._2 :+ t1 :+ t2)
  }
  */

  override def toString = s"if@$hashCode (${condition.toString})"
}
