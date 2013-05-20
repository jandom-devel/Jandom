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
import it.unich.sci.jandom.domains.NumericalProperty

/**
 * This class analyzes a method of a Java class. It uses the Jimple intermediate representation of the Soot library. It is
 * based on the generic analyzer for control flow graphs.
 * @param method the method we want to analyze
 * @author Gianluca Amato
 */
class JimpleMethod(method: SootMethod) extends SootCFG[JimpleMethod, Block] {
  import scala.collection.JavaConversions._

  /**
   * @inheritdoc
   * Here, we only handle numerical domains.
   */
  type DomainBase = NumericalDomain

  val body = method.retrieveActiveBody()
  val graph = new soot.jandom.BriefBigBlockGraph(body)
  val lastPP = Some(graph.getTails().get(0))

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
   * Analyze a `Value`.
   * @tparam Property the type of the abstract state
   * @param v the `Value` to analyze
   * @param prop the abstract initial state
   * @return the abstract end state. The last dimension of property corresponds to the
   * returned value.
   */
  def analyzeExpr[Property <: NumericalProperty[Property]](v: Value, prop: Property): Property = {
    v match {
      case v: IntConstant =>
        prop.addDimension.constantAssignment(c = v.value)
      case v: Local =>
        prop.addDimension.variableAssignment(m = envMap(v))
      case v: BinopExpr =>
        val res1 = analyzeExpr(v.getOp1(), prop)
        val res2 = analyzeExpr(v.getOp2(), res1)
        v match {
          case v: AddExpr => res2.variableAdd()
          case v: SubExpr => res2.variableSub()
          case v: MulExpr => res2.variableMul()
          case v: DivExpr => res2.variableDiv()
          case v: RemExpr => res2.variableRem()
          case v: ShlExpr => res2.variableShl()
          case v: ShrExpr => res2.variableShr()
          case v: UshrExpr => res2.variableUshr()

          // bitwise expressions (not supported yet)
          case v: AndExpr => res2.delDimension()
          case v: OrExpr => res2.delDimension()
          case v: XorExpr => res2.delDimension()

          // boolean expressions (not supported yet)
          case v: CmpExpr => res2.delDimension()
          case v: CmpgExpr => res2.delDimension()
          case v: CmplExpr => res2.delDimension()
          case v: ConditionExpr => res2.delDimension()
        }
      case v: UnopExpr =>
        val res = analyzeExpr(v.getOp(), prop)
        v match {
          case v: LengthExpr => prop.addDimension
          case v: NegExpr => prop.variableNeg()
        }
      case v: AnyNewExpr => prop.addDimension
      case v: InvokeExpr => prop.addDimension
      case v: InstanceOfExpr => prop.addDimension
      case v: CastExpr => prop.addDimension // TODO: this can be made more precise
      case _ =>
        throw new IllegalArgumentException("Invalid Jimple statement encountered")
    }
  }

  /**
   * Convert a `Value` into a LinearForm, if possible.
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

  protected def analyzeBlock(params: Parameters)(node: Block, initprop: params.Property): Seq[params.Property] = {
    var exits = Seq[params.Property]()
    var currprop = initprop
    for (unit <- node.iterator()) unit match {
      case unit: AssignStmt =>
        val local = unit.getLeftOp().asInstanceOf[Local]
        // try to get a linear expression, otherwise analyze piecewise
        val lf = jimpleExprToLinearForm(unit.getRightOp())
        lf match {
          case None =>
            currprop = analyzeExpr(unit.getRightOp(), currprop).variableAssignment(envMap(local), currprop.dimension).delDimension(currprop.dimension)
          case Some(a) =>
            currprop = currprop.linearAssignment(envMap(local), a.tail, a(0))
        }
      case unit: BreakpointStmt =>
        throw new IllegalArgumentException("Invalid Jimple statement encountered")
      case unit: DefinitionStmt =>
        throw new IllegalArgumentException("Unsupported Jimple statement")
      case unit: EnterMonitorStmt =>
      case unit: ExitMonitorStmt =>
      case unit: GotoStmt =>
        exits :+= currprop
      case unit: IfStmt =>
        val cond = unit.getCondition
        val lc = jimpleExprToLinearCond(cond)
        lc match {
          case None =>
            exits :+= currprop
          case Some(c) =>
            exits :+= c.analyze(currprop)
            currprop = c.opposite.analyze(currprop)
        }
      case unit: InvokeStmt =>
      case unit: LookupSwitchStmt =>
      case unit: NopStmt =>
      case unit: RetStmt =>
      case unit: ReturnStmt =>
      case unit: ReturnVoidStmt =>
      case unit: TableSwitchStmt =>
      case unit: ThrowStmt =>
    }
    if (node.getTail.fallsThrough()) exits +:= currprop
    exits
  }
}
