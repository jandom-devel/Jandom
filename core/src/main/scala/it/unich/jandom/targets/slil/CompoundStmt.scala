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
import it.unich.jandom.targets.{Annotation, lts}

import scala.collection.mutable

/**
  * A class for the compound statement (sequential composition). Each compound statements has
  * several program points, one at the beggining, another at the end, and one between each
  * pair of statements.
  *
  * @param stmts the sequence of statements that form the compound statement
  */
class CompoundStmt(val stmts: SLILStmt*) extends SLILStmt {

  import AnalysisPhase._

  def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase,
                                      ann: Annotation[ProgramPoint, params.Property]): params.Property = {
    var current = input
    for ((stmt, index) <- stmts.zipWithIndex) {
      if (index > 0 && params.allPPResult) ann((this, index)) = current
      current = stmt.analyzeStmt(params)(current, phase, ann)
    }
    current
  }

  def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint, U], ppspec: SLILPrinterSpec,
                                          row: Int, level: Int): String = {
    val spaces = ppspec.indent(level)
    val result = new StringBuilder()
    for ((stmt, index) <- stmts.zipWithIndex) {
      for (p <- ann.get((this, index))) {
        result ++= spaces + ppspec.decorator(p, row + result.count(_ == '\n'), spaces.length) + '\n'
      }
      result ++= stmt.mkString(ann, ppspec, row + result.count(_ == '\n'), level)
    }
    for (p <- ann.get((this, stmts.size))) {
      result ++= spaces + ppspec.decorator(p, row + result.count(_ == '\n'), spaces.length) + '\n'
    }
    result.toString
  }

  def syntacticallyEquals(that: SLILStmt): Boolean = that match {
    case that: CompoundStmt =>
      stmts.size == that.stmts.size &&
        (stmts zip that.stmts).forall(p => p._1 syntacticallyEquals p._2)
    case _: SLILStmt => false
  }

  val numvars: Int = (stmts map (_.numvars)).max

  def toLTS(prev: lts.Location, next: lts.Location): (Map[ProgramPoint, lts.Location], Seq[lts.Transition]) = {
    val transitions = mutable.ListBuffer.empty[lts.Transition]
    val locations = mutable.HashMap.empty[ProgramPoint, lts.Location]
    var src = prev
    var tgt = prev
    for ((stmt, index) <- stmts.zip(Stream from 1)) {
      tgt = if (index == stmts.size) next else lts.Location((this, index).toString, Seq.empty)
      if (index != stmts.size) locations((this, index)) = tgt
      val innerLTS = stmt.toLTS(src, tgt)
      locations ++= innerLTS._1
      transitions ++= innerLTS._2
      src = tgt
    }
    (locations.toMap, transitions.toList)
  }

  override def toString = s"sequence@$hashCode (${stmts.head} ...) len ${stmts.size}"
}

object CompoundStmt {
  def apply(stmts: SLILStmt*): CompoundStmt = new CompoundStmt(stmts: _*)
}
