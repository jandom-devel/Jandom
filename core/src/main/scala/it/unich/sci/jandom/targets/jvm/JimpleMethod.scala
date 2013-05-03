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

package it.unich.sci.jandom.targets.jvm

import java.io.PrintWriter
import java.io.StringWriter

import scala.Array.canBuildFrom
import scala.collection.mutable.HashMap

import it.unich.sci.jandom.domains.NumericalDomain
import it.unich.sci.jandom.domains.NumericalProperty
import it.unich.sci.jandom.targets._
import it.unich.sci.jandom.targets.cfg.ControlFlowGraph
import it.unich.sci.jandom.targets.linearcondition._

import soot._
import soot.baf._
import soot.jimple._
import soot.options.Options
import soot.tagkit.LoopInvariantTag
import soot.toolkits.graph._

/**
 * This class analyzes a method of a Java class. It uses the Jimple intermediate representation of the Soot library. It is
 * based on the generic analyzer for control flow graphs.
 * @param method the method we want to analyze
 * @author Gianluca Amato
 */
class JimpleMethod(method: SootMethod) extends SootCFG[JimpleMethod, Unit] {
  import scala.collection.JavaConversions._

  /**
   * @inheritdoc
   * The base node of this CFG is `Unit`.
   */
  type Node = Unit

  /**
   * @inheritdoc
   * Here, we only handle numerical domains.
   */
  type DomainBase = NumericalDomain

  val body = method.retrieveActiveBody()
  val graph = new ExceptionalUnitGraph(body)
  val lastPP = Some(body.getUnits().getLast())

  private val envMap = body.getLocals().zipWithIndex.toMap

  /**
   * Convert a `Value` into a LinearCond.
   * @param v the Value to convert.
   * @return the corresponding linear condition, or `None` if `v` is not a linear condition.
   */
  private def jimpleExprToLinearCond(v: Value): Option[LinearCond] = {
    import AtomicCond.ComparisonOperators
    val newcond = v match {
      case v: ConditionExpr =>
        val res1 = jimpleExprToLinearForm(v.getOp1())
        val res2 = jimpleExprToLinearForm(v.getOp2())
        res1 flatMap { res1 =>
          res2 flatMap { res2 =>
            // TODO: this is terrible... we need it because the linear form / linear cond API should be rewritten
            val lf = LinearForm(for (i <- 0 to size) yield res1(i) - res2(i))
            v match {
              case _: GtExpr => Some(AtomicCond(lf, AtomicCond.ComparisonOperators.GT))
              case _: GeExpr => Some(AtomicCond(lf, AtomicCond.ComparisonOperators.GTE))
              case _: LtExpr => Some(AtomicCond(lf, AtomicCond.ComparisonOperators.LT))
              case _: LeExpr => Some(AtomicCond(lf, AtomicCond.ComparisonOperators.LTE))
              case _: EqExpr => Some(AtomicCond(lf, AtomicCond.ComparisonOperators.EQ))
              case _ => None
            }
          }
        }
      case v: BinopExpr =>
        val res1 = jimpleExprToLinearCond(v.getOp1())
        val res2 = jimpleExprToLinearCond(v.getOp2())
        res1 flatMap { res1 =>
          res2 flatMap { res2 =>
            v match {
              case _: AndExpr => Some(AndCond(res1, res2))
              case _: OrExpr => Some(OrCond(res1, res2))
              case _ => None
            }
          }
        }
      case _ => None
    }
    newcond
  }

  /**
   * Convert a `Value` into a LinearForm.
   * @param v the Value to convert.
   * @return the corresponding linear form, or `None` if `v` is not a linear form.
   */
  def jimpleExprToLinearForm(v: Value): Option[Array[Double]] = {
    val a = Array.fill(size + 1)(0.0)
    var c = 0.0
    v match {
      case v: IntConstant =>
        a(0) = v.value
      case v: Local =>
        a(envMap(v) + 1) = 1
      case v: BinopExpr =>
        val res1 = jimpleExprToLinearForm(v.getOp1())
        val res2 = jimpleExprToLinearForm(v.getOp2())
        (res1, res2) match {
          case Tuple2(Some(a1), Some(a2)) =>
            v match {
              case v: AddExpr =>
                for (i <- 0 to size) a(i) = a1(i) + a2(i)
            }
          case _ => return None
        }
    }
    Some(a)
  }

  protected def analyzeBlock(params: Parameters)(node: Unit, prop: params.Property): Seq[params.Property] = {
    var exits = Seq[params.Property]()
    var newprop = prop
    node match {
      case unit: AssignStmt =>
        val local = unit.getLeftOp().asInstanceOf[Local]
        val lf = jimpleExprToLinearForm(unit.getRightOp())
        lf match {
          case None =>
            newprop = prop.nonDeterministicAssignment(envMap(local))
          case Some(a) =>
            newprop = prop.linearAssignment(envMap(local), a.tail, a(0))
        }
      case unit: IfStmt =>
        val cond = unit.getCondition
        val lc = jimpleExprToLinearCond(cond)
        lc match {
          case None =>
            exits :+= prop
          case Some(c) =>
            exits :+= c.analyze(prop)
            newprop = c.opposite.analyze(prop)
        }
      case unit: GotoStmt =>
        exits :+= prop
      case unit: ReturnVoidStmt =>
    }
    if (node.fallsThrough) exits +:= newprop
    exits
  }
}
