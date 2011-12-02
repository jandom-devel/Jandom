/**
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
 *
 * (c) 2011 Gianluca Amato
 */

package it.unich.sci.jandom.targets

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

sealed abstract class SLILCond
case class AtomicCond[T](lf: LinearForm[T], op: ComparisonOperators.Value) extends SLILCond
case class AndCond(cond1: SLILCond, cond2: SLILCond) extends SLILCond
case class OrCond(cond1: SLILCond, cond2: SLILCond) extends SLILCond
case class NotCond(cond: SLILCond) extends SLILCond

sealed abstract class SLILStmt 
case class AssignStmt[T](variable: Int, linearform: LinearForm[T]) extends SLILStmt
case class CompoundStmt(stmts: List[SLILStmt]) extends SLILStmt
case class WhileStmt(condition: SLILCond, body: SLILStmt) extends SLILStmt
case class IfStmt(condition: SLILCond, if_branch: SLILStmt, else_branch: SLILStmt) extends SLILStmt
case class NopStmt() extends SLILStmt

case class SLILProgram(variableNames: Iterable[String], inputVars: Iterable[Int], stmt: SLILStmt)  
