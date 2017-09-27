/**
 * Copyright 2013, 2016 Gianluca Amato, Francesca Scozzari
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

package it.unich.jandom.targets.jvmsoot

import scala.collection.JavaConverters._

import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.targets._
import it.unich.jandom.targets.NumericCondition._

import soot._
import soot.jimple._
import soot.toolkits.graph._
import spire.math.Rational

/**
 * This class analyzes a method of a Java class. It uses the Jimple intermediate representation of the Soot library. It is
 * based on the generic analyzer for control flow graphs.
 * @param method the method we want to analyze
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
class JimpleMethod(method: SootMethod) extends SootCFG[JimpleMethod, Block](method) {
  val body = method.retrieveActiveBody()
  val graph = new soot.jandom.UnitBlockGraph(body)

  protected def analyzeBlock(params: Parameters)(node: Block, initprop: params.Property): Seq[params.Property] = {
    /*
     * Convert a `Value` into a LinearForm, if possible.
     * @param v the Value to convert.
     * @return the corresponding linear form, or `None` if `v` is not a linear form.
     */
    def jimpleExprToLinearForm(v: Value): Option[Array[Rational]] = {
      val a = Array.fill(size + 1)(Rational.zero)
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
            case _ => None
          }
        case _ => None
      }
      Some(a)
    }

    /*
     * Convert a `Value` into a LinearCond.
     * @param v the Value to convert.
     * @return the corresponding linear condition, or `None` if `v` is not a linear condition.
     */
    def jimpleExprToLinearCond(v: Value): Option[NumericCondition] = {
      val newcond = v match {
        case v: ConditionExpr =>
          val res1 = jimpleExprToLinearForm(v.getOp1())
          val res2 = jimpleExprToLinearForm(v.getOp2())
          res1 flatMap { res1 =>
            res2 flatMap { res2 =>
              val lf = LinearForm(res1: _*) - LinearForm(res2: _*)
              v match {
                case _: GtExpr => Some(AtomicCond(lf, ComparisonOperators.GT))
                case _: GeExpr => Some(AtomicCond(lf, ComparisonOperators.GTE))
                case _: LtExpr => Some(AtomicCond(lf, ComparisonOperators.LT))
                case _: LeExpr => Some(AtomicCond(lf, ComparisonOperators.LTE))
                case _: EqExpr => Some(AtomicCond(lf, ComparisonOperators.EQ))
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

    /*
     * Analyze a boolean expression in the guard of an If statement. It tries to convert v into a linear
     * condition. If it does not succeed, analyzes it recursively.
     * @param v the `Value` to analyze
     * @param prop the abstract initial state
     * @return the abstract end states. The first state is for the "then"-branch, the second
     * state is for the "else"-branch.
     */
    def analyzeCond(v: Value, prop: params.Property): (params.Property, params.Property) = {
      val lc = if (v.getType().isInstanceOf[PrimType])
        jimpleExprToLinearCond(v)
      else
        None
      lc match {
        case Some(lc) =>
          prop.testLinearCondition(lc)
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

    /*
     * Analyze an invocation in Jimple.
     * @param v the invoke expression to analyze
     * @param prop the abstract initial state
     * @return the abstract end state. The last dimension of property corresponds to the
     * returned value.
     */
    def analyzeInvokeExpr(v: InvokeExpr, prop: params.Property): params.Property = {
      import scala.language.existentials
      val method = v.getMethod()
      val (baseprop, implicitArgs) = v match {
        case v: InstanceInvokeExpr =>
          (analyzeExpr(v.getBase(), prop), 1)
        case v: StaticInvokeExpr =>
          (prop, 0)
        case v: DynamicInvokeExpr =>
          throw new IllegalArgumentException("Invoke dynamic not yet supported")
      }
      val callprop = v.getArgs().asScala.foldLeft(baseprop) { case (p, arg) => analyzeExpr(arg, p) }
      val inputprop = callprop.extract(v.getArgCount() + implicitArgs)
      val exitprop = params.interpretation match {
        case Some(inte) => inte(method, inputprop)
        case None => throw new IllegalArgumentException("Interprocedural analysis")
      }
      callprop.connect(exitprop, method.getParameterCount() + implicitArgs)
    }

    /*
     * Analyze an expression in Jimple.
     * @param v the `Value` to analyze
     * @param prop the abstract initial state
     * @return the abstract end state
     */
    def analyzeExpr(v: Value, prop: params.Property): params.Property = {
      v match {
        case v: NullConstant =>
          prop.evalNull()
        case v: StringConstant =>
          prop.evalGlobal(v)
        case v: IntConstant =>
          prop.evalConstant(v.value)
        case v: StaticFieldRef =>
          prop.evalStaticField(v.getField())
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
          v match {
            case v: LengthExpr => prop.evalLength
            case v: NegExpr => prop.evalNeg
          }
        case v: AnyNewExpr => prop.evalNew(v.getType())
        case v: InvokeExpr => analyzeInvokeExpr(v, prop)
        case v: InstanceOfExpr => prop.evalNull()
        case v: CastExpr => prop.evalCast(v.getCastType())
        case v: InstanceFieldRef => prop.evalField(localMap(v.getBase().asInstanceOf[Local]), v.getField())
      }
    }

    var exits = Seq[params.Property]()
    var currprop = initprop
    for (unit <- node.asScala)
      unit match {
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
          val expr = unit.getRightOp() match {
            case v: ParameterRef =>
              // we assume that the ordering is: @this, @parameter0, parameter1, ...
              currprop.evalLocal(v.getIndex + (if (method.isStatic()) 0 else 1))
            case v: ThisRef =>
              // we assume that @this is in position 0
              currprop.evalLocal(0)
            case _ =>
              throw new UnsupportedSootUnitException(unit)
          }
          currprop = unit.getLeftOp() match {
            case local: Local =>
              expr.assignLocal(localMap(local))
            case field: InstanceFieldRef =>
              val local = field.getBase().asInstanceOf[Local]
              expr.assignField(localMap(local), field.getField())
          }
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
          currprop = analyzeInvokeExpr(unit.getInvokeExpr(), currprop)
          if (unit.getInvokeExpr().getType() != VoidType.v()) currprop = currprop.restrict(1)
        case unit: LookupSwitchStmt =>
          throw new UnsupportedSootUnitException(unit)
        case unit: NopStmt =>
        case unit: RetStmt =>
          throw new UnsupportedSootUnitException(unit)
        case unit: ReturnStmt =>
          // ReturnStmt is implemented by evaluating the returned expression. In this way, the
          // obtained abstract state has one dimension more than the other program points.
          exits :+= analyzeExpr(unit.getOp(), currprop)
        case unit: ReturnVoidStmt =>
          exits :+= currprop
        case unit: TableSwitchStmt =>
          throw new UnsupportedSootUnitException(unit)
        case unit: ThrowStmt =>
          throw new UnsupportedSootUnitException(unit)
      }
    if (node.getTail.fallsThrough()) exits +:= currprop
    exits
  }
}
