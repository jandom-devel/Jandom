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

package it.unich.sci.jandom.targets.jvmsoot

import java.io.PrintWriter
import java.io.StringWriter

import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

import it.unich.sci.jandom.domains.NumericalDomain
import it.unich.sci.jandom.domains.NumericalProperty
import it.unich.sci.jandom.targets._
import it.unich.sci.jandom.targets.linearcondition._

import soot._
import soot.baf._
import soot.jimple._
import soot.options.Options
import soot.tagkit.LoopInvariantTag
import soot.toolkits.graph._

/**
 * Analysis of a method using the Jimple intermediate representation.
 * @author Gianluca Amato
 *
 */
class JimpleMethod(method: SootMethod) extends Target {
  import scala.collection.JavaConversions._

  type ProgramPoint = Unit
  type Tgt = JimpleMethod
  type DomainBase = NumericalDomain

  val body = method.retrieveActiveBody()
  val chain = body.getUnits()
  val locals = body.getLocals()
  val localsList = locals.iterator.toArray map { _.getName() }
  val envMap = new HashMap[Local, Int]

  val i = locals.iterator
  for (n <- 0 until locals.size) {
    envMap(i.next()) = n
  }
  val order = weakTopologicalOrder

  def weakTopologicalOrder: Annotation[ProgramPoint, Integer] = {
    val cfg = new ExceptionalUnitGraph(body)
    val order = new PseudoTopologicalOrderer[Unit].newList(cfg, false)
    val ann = getAnnotation[Integer]
    var index = 0
    order.iterator.foreach { u => ann(u) = index; index += 1 }
    ann
  }

  private def jimpleExprToLinearCond(v: Value): Option[LinearCond] = {
    import AtomicCond.ComparisonOperators
    val newcond = v match {
      case v: ConditionExpr =>
        val res1 = jimpleExprToLinearForm(v.getOp1())
        val res2 = jimpleExprToLinearForm(v.getOp2())
        res1 flatMap { res1 =>
          res2 flatMap { res2 =>
            // this is terrible... we need it because the linear form / linear cond
            // API should be rewritten
            val lf = LinearForm(for (i <- 0 to locals.size) yield res1(i) - res2(i))
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

  def jimpleExprToLinearForm(v: Value): Option[Array[Double]] = {
    val a = Array.fill(locals.size() + 1)(0.0)
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
                for (i <- 0 to locals.size) a(i) = a1(i) + a2(i)
            }
          case _ => return None
        }
    }
    Some(a)
  }

  def analyzeBlock[Property <: NumericalProperty[Property]](pp: ProgramPoint, ann: Annotation[ProgramPoint, Property]): Seq[(ProgramPoint, Property)] = {

    var exits = Seq[(ProgramPoint, Property)]()
    var state = ann(pp)
    var unit = pp
    var nextunit = pp
    do {
      unit = nextunit
      unit match {
        case unit: AssignStmt =>
          val local = unit.getLeftOp().asInstanceOf[Local]
          val lf = jimpleExprToLinearForm(unit.getRightOp())
          lf match {
            case None =>
              state = state.nonDeterministicAssignment(envMap(local))
            case Some(a) =>
              state = state.linearAssignment(envMap(local), a.tail, a(0))
          }
        case unit: IfStmt =>
          val cond = unit.getCondition
          val lc = jimpleExprToLinearCond(cond)
          lc match {
            case None =>
              exits :+= (unit.getTarget, state)
            case Some(c) =>
              exits :+= (unit.getTarget, c.analyze(state))
              state = c.opposite.analyze(state)
          }
        case unit: GotoStmt =>
          exits :+= (unit.getTarget, state)

        case unit: ReturnVoidStmt =>
          ann(unit) = state
      }
      // We use the chain to get the fall through node. We used the first successor
      // in the CFG, but it is not clear from the documentation if we can rely on
      // this assumption.
      nextunit = chain.getSuccOf(unit)
    } while (unit.fallsThrough() && nextunit.getBoxesPointingToThis().isEmpty())
    if (unit.fallsThrough) exits :+= (nextunit, state)
    exits
  }

  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property] = {
    val ann = getAnnotation[params.Property]
    val taskList = Queue[ProgramPoint](chain.getFirst())
    ann(chain.getFirst()) = params.domain.full(body.getLocalCount())
    val annEdge = HashMap[ProgramPoint, HashMap[ProgramPoint, params.Property]]()
    while (!taskList.isEmpty) {
      val pp = taskList.dequeue()
      val result = analyzeBlock(pp, ann)
      for ((destpp, state) <- result) {
        annEdge.getOrElseUpdate(destpp, HashMap[ProgramPoint, params.Property]()).update(pp, state)
        if (ann contains destpp) {
          val oldstate = ann(destpp)
          val newstate = if (order(destpp) <= order(pp))
            ann(destpp) widening state
          else
            ann(destpp) union state
          if (newstate > oldstate) {
            ann(destpp) = newstate
            taskList.enqueue(destpp)
          }
        } else {
          ann(destpp) = state
          taskList.enqueue(destpp)
        }
      }
    }
    taskList.enqueue(chain.getFirst())    
    while (!taskList.isEmpty) {
      val pp = taskList.dequeue()
      val result = analyzeBlock(pp, ann)
      for ((destpp, state) <- result) {
        annEdge(destpp)(pp) = state
        val base = annEdge(destpp).foldRight(state.empty)(_._2 union _)
        val oldstate = ann(destpp)
        val newstate = ann(destpp) intersection base
        if (newstate < oldstate) {
          ann(destpp) = newstate
          taskList.enqueue(destpp)
        }
      }
    }
    ann
  }

  def mkString[D <: NumericalProperty[D]](ann: Annotation[ProgramPoint, D]): String = {
    for ((unit, prop) <- ann) {
      unit.addTag(new LoopInvariantTag("[ " + prop.mkString(localsList).mkString(", ") + " ]"))
    }
    Options.v().set_print_tags_in_output(true)
    val printer = Printer.v()
    val ss = new StringWriter()
    val ps = new PrintWriter(ss)
    printer.printTo(body, ps)
    ps.close()
    val out = ss.getBuffer.toString

    val lines = out.split("\n")
    for (i <- 0 until lines.length) {
      if (lines(i).startsWith("/*") && i > 0 && !lines(i - 1).startsWith("/*")) {
        val temp = lines(i)
        lines(i) = lines(i - 1)
        lines(i - 1) = temp
      }
    }
    for ((unit, prop) <- ann)
      unit.removeAllTags()
    lines.mkString("\n")
  }

  override def toString = mkString(getAnnotation[Null])
}
