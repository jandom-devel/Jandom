/* Copyright 2013, 2018 Gianluca Amato, Francesca Scozzari
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

import scala.jdk.CollectionConverters._

import soot._
import soot.baf._
import soot.jimple._
import soot.toolkits.graph._

/**
  * This class analyzes a method of a Java class. It uses the Baf intermediate representation of the Soot library.  It is
  * based on the generic analyzer for control flow graphs.
  *
  * @param method the method we want to analyze
  * @param io     whether I/O semantics or standard semantics is desired. It should agree with the value of `params.io` in
  *               methods which requires parameters.
  * @author Gianluca Amato
  * @author Luca Mangifesta
  */
class BafMethod(method: SootMethod, io: Boolean) extends SootCFG[BafMethod, Block](method, io) {
  val body: Body = Baf.v().newBody(method.retrieveActiveBody().asInstanceOf[JimpleBody])
  val graph = new soot.jandom.UnitBlockGraph(body)

  /**
    * @note In developing this method we are assuming that, if a unit has a fall-through, it is the first
    *       successor returned by `getSuccsOf`.
    */
  protected def analyzeBlock(params: Parameters)(node: Block, initprop: params.Property): Seq[params.Property] = {
    var exits = Seq[params.Property]()
    var currprop = initprop

    def analyzeTargetArgInst(unit: TargetArgInst, prop: params.Property) = unit match {
      case _: GotoInst => (prop, prop)
      case _: JSRInst => (prop, prop)
      case _: IfCmpLtInst => prop.testLt
      case _: IfCmpGtInst => prop.testGt
      case _: IfCmpLeInst => prop.testLe
      case _: IfCmpGeInst => prop.testGe
      case _: IfCmpEqInst => prop.testEq
      case _: IfCmpNeInst => prop.testNe
      case _: IfLtInst => prop.evalConstant(0).testLt
      case _: IfGtInst => prop.evalConstant(0).testGt
      case _: IfLeInst => prop.evalConstant(0).testLe
      case _: IfGeInst => prop.evalConstant(0).testGe
      case _: IfEqInst => prop.evalConstant(0).testEq
      case _: IfNeInst => prop.evalConstant(0).testNe
      case _: IfNonNullInst => prop.evalNull().testNe
      case _: IfNullInst => prop.evalNull().testEq
    }

    for (unit <- node.iterator().asScala)
      currprop = unit match {
        case unit: AddInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalAdd
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: AndInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalBinOp
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case _: ArrayLengthInst =>
          currprop.evalLength
        case _: ArrayReadInst =>
          throw UnsupportedSootUnitException(unit)
        case _: ArrayWriteInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: CmpgInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalBinOp
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: CmpInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalBinOp
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: CmplInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalBinOp
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: DivInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalDiv
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: Dup1_x1Inst =>
          unit.getOp1Type match {
            case _: PrimType => unit.getUnder1Type match {
              case _: PrimType => currprop.evalDup1_x1()
              case _: Type => throw UnsupportedSootUnitException(unit)
            }
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: Dup1_x2Inst =>
          unit.getOp1Type match {
            case _: PrimType => unit.getUnder1Type match {
              case _: LongType => currprop.evalDup1_x1()
              case _: DoubleType => currprop.evalDup1_x1()
              case _: PrimType => unit.getUnder2Type match {
                case _: PrimType => currprop.evalDup1_x2()
                case _: Type => throw UnsupportedSootUnitException(unit)
              }
              case _: Type => throw UnsupportedSootUnitException(unit)
            }
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: Dup1Inst =>
          unit.getOp1Type match {
            case _: PrimType => currprop.evalDup1()
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: Dup2_x1Inst =>
          unit.getOp1Type match {
            case _: LongType => unit.getOp2Type match {
              case _: PrimType => currprop.evalDup1_x1()
              case _: Type => throw UnsupportedSootUnitException(unit)
            }
            case _: DoubleType => unit.getOp2Type match {
              case _: PrimType => currprop.evalDup1_x1()
              case _: Type => throw UnsupportedSootUnitException(unit)
            }
            case _: PrimType => unit.getOp2Type match {
              case _: PrimType => unit.getUnder1Type match {
                case _: PrimType => currprop.evalDup2_x1()
                case _: Type => throw UnsupportedSootUnitException(unit)
              }
              case _: Type => throw UnsupportedSootUnitException(unit)
            }
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: Dup2_x2Inst =>
          unit.getOp1Type match {
            case _: LongType => unit.getOp2Type match {
              case _: PrimType => unit.getUnder1Type match {
                case _: PrimType => currprop.evalDup1_x2()
                case _: Type => throw UnsupportedSootUnitException(unit)
              }
              case _: Type => throw UnsupportedSootUnitException(unit)
            }
            case _: DoubleType => unit.getOp2Type match {
              case _: PrimType => unit.getUnder1Type match {
                case _: PrimType => currprop.evalDup1_x2()
                case _: Type => throw UnsupportedSootUnitException(unit)
              }
              case _: Type => throw UnsupportedSootUnitException(unit)
            }
            case _: PrimType => unit.getOp2Type match {
              case _: PrimType => unit.getUnder1Type match {
                case _: PrimType => unit.getUnder2Type match {
                  case _: PrimType => currprop.evalDup2_x2()
                  case _: Type => throw UnsupportedSootUnitException(unit)
                }
                case _: Type => throw UnsupportedSootUnitException(unit)
              }
              case _: Type => throw UnsupportedSootUnitException(unit)
            }
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: Dup2Inst =>
          unit.getOp1Type match {
            case _: LongType => currprop.evalDup1()
            case _: DoubleType => currprop.evalDup1()
            case _: PrimType => unit.getOp2Type match {
              case _: PrimType => currprop.evalDup2()
              case _: Type => throw UnsupportedSootUnitException(unit)
            }
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case _: DynamicInvokeInst =>
          throw UnsupportedSootUnitException(unit)
        case _: EnterMonitorInst =>
          currprop.enterMonitor()
        case _: ExitMonitorInst =>
          currprop.exitMonitor()
        case unit: FieldGetInst =>
          currprop.evalField(f = unit.getField)
        case unit: FieldPutInst =>
          currprop.assignField(f = unit.getField)
        case _: IdentityInst =>
          currprop
        case unit: IncInst =>
          unit.getConstant match {
            case i: IntConstant => currprop.evalInc(localMap(unit.getLocal), i.value)
            case i: FloatConstant => currprop.evalInc(localMap(unit.getLocal), i.value)
            case i: DoubleConstant => currprop.evalInc(localMap(unit.getLocal), i.value)
            case _: Constant => throw UnsupportedSootUnitException(unit)
          }
        case _: InstanceCastInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: InstanceOfInst =>
          currprop.evalInstance(unit.getCheckType)
        case unit: InterfaceInvokeInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: LoadInst =>
          currprop.evalLocal(localMap(unit.getLocal))
        case _: LookupSwitchInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: MulInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalMul
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: NegInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalNeg
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case _: NewArrayInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: NewInst =>
          currprop.evalNew(unit.getBaseType)
        case _: NewMultiArrayInst =>
          throw UnsupportedSootUnitException(unit)
        case _: NopInst =>
          currprop
        case unit: OrInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalBinOp
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: PopInst =>
          currprop.restrict(unit.getWordCount)
        case _: PrimitiveCastInst =>
          currprop
        case unit: PushInst =>
          unit.getConstant match {
            case i: IntConstant => currprop.evalConstant(i.value)
            case i: FloatConstant => currprop.evalConstant(i.value)
            case i: DoubleConstant => currprop.evalConstant(i.value)
            case _: NullConstant => currprop.evalNull()
            case i: StringConstant => currprop.evalConstant(i.value)
            case _: Constant => throw UnsupportedSootUnitException(unit)
          }
        case unit: RemInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalRem
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case _: RetInst =>
          exits :+= currprop
          currprop
        case _: ReturnInst =>
          exits :+= currprop
          currprop
        case _: ReturnVoidInst =>
          exits :+= currprop
          currprop
        case unit: ShlInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalShl
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case unit: ShrInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalShr
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case _: SpecialInvokeInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: StaticGetInst =>
          currprop.evalStaticField(unit.getField)
        case _: StaticInvokeInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: StaticPutInst =>
          currprop.assignStaticField(f = unit.getField)
        case unit: StoreInst =>
          currprop.assignLocal(localMap(unit.getLocal))
        case unit: SubInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalSub
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case _: SwapInst =>
          currprop.evalSwap()
        case _: TableSwitchInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: TargetArgInst =>
          // le istruzioni if<cond> assumiamo che il valore di confronto sia intero
          val (tbranch, fbranch) = analyzeTargetArgInst(unit, currprop)
          exits :+= tbranch
          fbranch
        case _: ThrowInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: UshrInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalUshr
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case _: VirtualInvokeInst =>
          throw UnsupportedSootUnitException(unit)
        case unit: XorInst =>
          unit.getOpType match {
            case _: PrimType => currprop.evalBinOp
            case _: Type => throw UnsupportedSootUnitException(unit)
          }
        case _: Inst =>
          throw UnsupportedSootUnitException(unit)
      }
    if (node.getTail.fallsThrough()) exits +:= currprop
    exits
  }
}
