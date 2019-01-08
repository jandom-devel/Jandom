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
    var index = 0
    for (stmt <- stmts) {
      if (params.allPPResult) ann((this, index)) = current
      index += 1
      current = stmt.analyzeStmt(params)(current, phase, ann)
    }
    if (params.allPPResult) ann((this, index)) = current
    current
  }

  def mkString[U <: NumericalProperty[_]](ann: Annotation[ProgramPoint, U], ppspec: SLILPrinterSpec,
                                          row: Int, level: Int): String = {
    val spaces = ppspec.indent(level)
    val result = new StringBuilder()
    var index = 0
    var decorations = 0
    for (stmt <- stmts) {
      for (p <- ann.get((this, index)); deco <- ppspec.decorator(p, row + result.count(_ == '\n'), spaces.length)) {
        result ++= spaces + deco + '\n'
        decorations += 1
      }
      result ++= stmt.mkString(ann, ppspec, row + result.count(_ == '\n'), level)
      index += 1
    }
    for (p <- ann.get((this, index)); deco <- ppspec.decorator(p, row + result.count(_ == '\n'), spaces.length)) {
      result ++= spaces + deco + '\n'
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
    locations((this, 0)) = prev
    var src = prev
    var tgt = prev
    var index = 1
    for (stmt <- stmts) {
      tgt = if (index == stmts.size) next else lts.Location((this, index).toString, Seq.empty)
      locations((this, index)) = tgt
      val innerLTS = stmt.toLTS(src, tgt)
      locations ++= innerLTS._1
      transitions ++= innerLTS._2
      index += 1
      src = tgt
    }
    (locations.toMap, transitions.toList)
  }

  override def toString = s"sequence@$hashCode (${stmts.head} ...) len ${stmts.size}"
}

object CompoundStmt {
  def apply(stmts: SLILStmt*): CompoundStmt = new CompoundStmt(stmts: _*)
}
