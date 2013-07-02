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

import it.unich.sci.jandom.targets._
import it.unich.sci.jandom.targets.linearcondition._

import soot._
import soot.jimple._
import soot.toolkits.graph._

/**
 * This class analyzes a method of a Java class. It uses the Jimple intermediate representation of the Soot library. It is
 * based on the generic analyzer for control flow graphs.
 * @param method the method we want to analyze
 * @author Gianluca Amato
 */
class JimpleMethod(method: SootMethod) extends SootCFG[JimpleMethod, Block](method) {
  import scala.collection.JavaConversions._

  val body = method.retrieveActiveBody()
  val graph = new soot.jandom.UnitBlockGraph(body)

  protected def analyzeBlock(params: Parameters)(node: Block, initprop: params.Property): Seq[params.Property] = {

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
        a(localMap(v) + 1) = 1
      case v: BinopExpr =>
        val res1 = jimpleExprToLinearForm(v.getOp1())
        val res2 = jimpleExprToLinearForm(v.getOp2())
        (res1, res2) match {
          case Tuple2(Some(a1), Some(a2)) =>
            v match {
              case v: AddExpr =>
                for (i <- 0 to size) a(i) = a1(i) + a2(i)
              case _ =>
                None
            }
          case _ => return None
        }
    }
    Some(a)
  }

  /**
   * Convert a `Value` into a LinearCond.
   * @param v the Value to convert.
   * @return the corresponding linear condition, or `None` if `v` is not a linear condition.
   */
  def jimpleExprToLinearCond(v: Value): Option[LinearCond] = {
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

  def analyzeCond(v: Value, prop: params.Property): (params.Property, params.Property) = {
    val lc = jimpleExprToLinearCond(v)
    lc match {
      case Some(lc) => prop.testLinearCondition(lc)
      case None =>
        v match {
          case v: ConditionExpr =>
            val res1 = analyzeExpr(v.getOp1(), prop)
            val res2 = analyzeExpr(v.getOp2(), res1)
            v match {
              case v: GtExpr => res2.testGt
              case v: GeExpr => res2.testGe
              case v: LtExpr => res2.testLt
              case v: LeExpr => res2.testLe
              case v: EqExpr => res2.testEq
              case v: NeExpr => res2.testNe
            }
        }
    }
  }

  def analyzeInvokeExpr(v: InvokeExpr, prop: params.Property): params.Property = {
    val method = v.getMethod()
    val (baseprop, implicitArgs) = v match {
      case v: InstanceInvokeExpr =>
        (analyzeExpr(v.getBase(), prop), 1)
      case v: StaticInvokeExpr =>
        (prop, 0)
      case v: DynamicInvokeExpr =>
        throw new IllegalArgumentException("Invoke dynamic not yet supported")
    }
    val callprop = v.getArgs().foldLeft(prop) { case (p, arg) => analyzeExpr(arg, p) }
    val inputprop = callprop.restrict(v.getArgCount() + implicitArgs)
    val exitprop = params.interpretation match  {
      case Some(inte) => inte(method, inputprop)
      case None => throw new IllegalArgumentException("Interprocedural analysis")
    }
    callprop.connect(exitprop, method.getParameterCount())
  }

  /**
   * Analyze a `Value`.
   * @tparam Property the type of the abstract state
   * @param v the `Value` to analyze
   * @param prop the abstract initial state
   * @return the abstract end state. The last dimension of property corresponds to the
   * returned value.
   */
  def analyzeExpr(v: Value, prop: params.Property): params.Property = {
    v match {
      case v: IntConstant =>
        prop.evalConstant(v.value)
      case v: Local =>
        prop.evalLocal(localMap(v))
      case v: BinopExpr =>
        val res1 = analyzeExpr(v.getOp1(), prop)
        val res2 = analyzeExpr(v.getOp2(), res1)
        v match {
          case v: AddExpr => res2.evalAdd
          case v: SubExpr => res2.evalSub
          case v: MulExpr => res2.evalMul
          case v: DivExpr => res2.evalDiv
          case v: RemExpr => res2.evalRem
          case v: ShlExpr => res2.evalShl
          case v: ShrExpr => res2.evalShr
          case v: UshrExpr => res2.evalUshr

          // bitwise expressions (not supported yet)
          case v: AndExpr => res2.evalBinOp
          case v: OrExpr => res2.evalBinOp
          case v: XorExpr => res2.evalBinOp

          // boolean expressions (not supported yet)
          case v: CmpExpr => res2.evalBinOp
          case v: CmpgExpr => res2.evalBinOp
          case v: CmplExpr => res2.evalBinOp

          case v: GtExpr => res2.evalGt
          case v: GeExpr => res2.evalGe
          case v: LtExpr => res2.evalLt
          case v: LeExpr => res2.evalLe
          case v: EqExpr => res2.evalEq
          case v: NeExpr => res2.evalNe
        }
      case v: UnopExpr =>
        val res = analyzeExpr(v.getOp(), prop)
        v match {
          case v: LengthExpr => prop.evalLength
          case v: NegExpr => prop.evalNeg
        }
      case v: AnyNewExpr => prop.evalNew(v.getType())
      case v: InvokeExpr => analyzeInvokeExpr(v, prop)
      case v: InstanceOfExpr => prop.evalNull
      case v: CastExpr => prop.evalNull // TODO: this can be made more precise
      case v: InstanceFieldRef => prop.evalField(localMap(v.getBase().asInstanceOf[Local]), v.getField())
    }
  }

    var exits = Seq[params.Property]()
    var currprop = initprop
    for (unit <- node.iterator()) unit match {
      case unit: AssignStmt =>
        val expr = analyzeExpr(unit.getRightOp(), currprop)
        unit.getLeftOp() match {
          case local: Local =>
            currprop = expr.assignLocal(localMap(local))
          case field: InstanceFieldRef =>
            val local = field.getBase().asInstanceOf[Local]
            currprop = expr.assignField(localMap(local), field.getField())
        }
      case unit: BreakpointStmt =>
        throw new UnsupportedSootUnitException(unit)
      case unit: IdentityStmt =>
      // ignore this instruction...
      case unit: EnterMonitorStmt =>
        unit.getOp() match {
          case local: Local =>
            currprop = currprop.enterMonitor(localMap(local))
        }
      case unit: ExitMonitorStmt =>
        unit.getOp() match {
          case local: Local =>
            currprop = currprop.exitMonitor(localMap(local))
        }
      case unit: GotoStmt =>
        exits :+= currprop
      case unit: IfStmt =>
        val (tbranch, fbranch) = analyzeCond(unit.getCondition(), currprop)
        exits :+= tbranch
        currprop = fbranch
      case unit: InvokeStmt =>
        currprop = analyzeExpr(unit.getInvokeExpr(), currprop)
      case unit: LookupSwitchStmt =>
        throw new UnsupportedSootUnitException(unit)
      case unit: NopStmt =>
      case unit: RetStmt =>
        throw new UnsupportedSootUnitException(unit)
      case unit: ReturnStmt =>
        val exitProp = if (params.io){
        	val e1 = analyzeExpr(unit.getOp(), currprop)
        	val e2 = e1.restrict(method.getParameterCount()+1)
        	e2
        } else
            currprop
        exits :+= exitProp
      case unit: ReturnVoidStmt =>
         val exitProp = if (params.io)
        	currprop.restrict(method.getParameterCount())
        else
            currprop
        exits :+= exitProp
      case unit: TableSwitchStmt =>
        throw new UnsupportedSootUnitException(unit)
      case unit: ThrowStmt =>
        throw new UnsupportedSootUnitException(unit)
    }
    if (node.getTail.fallsThrough()) exits +:= currprop
    exits
  }
}
