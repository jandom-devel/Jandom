/* Copyright 2013 Gianluca Amato
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

import it.unich.sci.jandom.targets.linearcondition.AtomicCond

import soot._
import soot.baf._
import soot.jimple._
import soot.toolkits.graph._

/**
 * This class analyzes a method of a Java class. It uses the Baf intermediate representation of the Soot library.  It is
 * based on the generic analyzer for control flow graphs.
 * @param method the method we want to analyze
 * @author Gianluca Amato
 */
class BafMethod(method: SootMethod) extends SootCFG[BafMethod, Block](method) {
  import scala.collection.JavaConversions._

  val body = Baf.v().newBody(method.retrieveActiveBody())
  val graph = new soot.jandom.UnitBlockGraph(body)

  /**
   * @note In developing this method we are assuming that, if a unit has a fall-through, it is the first
   * successor returned by `getSuccsOf`.
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
    }

    for (unit <- node.iterator())
      currprop = unit match {
        case unit: AddInst =>
        unit.getOpType() match {
          case i: PrimType => currprop.evalAdd
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: AndInst =>
        unit.getOpType() match{
          case i: PrimType => currprop.evalBinOp
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: ArrayLengthInst =>
        currprop.evalLength
      case unit: ArrayReadInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: ArrayWriteInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: CmpgInst =>
      	unit.getOpType() match {
      	  case i: PrimType => currprop.evalBinOp
      	  case i: Type => throw UnsupportedSootUnitException(unit)
      	}
      case unit: CmpInst =>
      	unit.getOpType() match {
      	  case i: PrimType => currprop.evalBinOp
      	  case i: Type => throw UnsupportedSootUnitException(unit)
      	}
      case unit: CmplInst =>
      	unit.getOpType() match {
      	  case i: PrimType => currprop.evalBinOp
      	  case i: Type => throw UnsupportedSootUnitException(unit)
      	}
      case unit: DivInst =>
        unit.getOpType() match {
          case i: PrimType => currprop.evalDiv
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: Dup1_x1Inst =>
        unit.getOp1Type() match {
          case i: PrimType => unit.getUnder1Type() match {
            case j: PrimType => currprop.evalDup1_x1
            case j: Type => throw UnsupportedSootUnitException(unit)
          }
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: Dup1_x2Inst =>
        unit.getOp1Type() match {
          case i: PrimType => unit.getUnder1Type() match {
            case j: LongType => currprop.evalDup1_x1
            case j: DoubleType => currprop.evalDup1_x1
            case j: PrimType => unit.getUnder2Type() match {
              case k: PrimType => currprop.evalDup1_x2
              case k: Type => throw UnsupportedSootUnitException(unit)
            }
            case j: Type => throw UnsupportedSootUnitException(unit)
          }
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: Dup1Inst =>
        unit.getOp1Type() match {
          case i: PrimType => currprop.evalLocal(size-1)
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: Dup2_x1Inst =>
        unit.getOp1Type() match {
          case i: LongType => unit.getOp2Type() match {
            case j: PrimType => currprop.evalDup1_x1
            case j: Type => throw UnsupportedSootUnitException(unit)
          }
          case i: DoubleType => unit.getOp2Type() match {
            case j: PrimType => currprop.evalDup1_x1
            case j: Type => throw UnsupportedSootUnitException(unit)
          }
          case i: PrimType => unit.getOp2Type() match {
            case j: PrimType => unit.getUnder1Type() match{
               case k: PrimType => currprop.evalDup2_x1
               case k: Type => throw UnsupportedSootUnitException(unit)
            }
            case j: Type => throw UnsupportedSootUnitException(unit)
          }
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: Dup2_x2Inst =>
        unit.getOp1Type() match {
          case i: LongType => unit.getOp2Type() match {
            case j: PrimType => unit.getUnder1Type() match{
              case k: PrimType => currprop.evalDup1_x2
              case k: Type => throw UnsupportedSootUnitException(unit)
            }
            case j: Type => throw UnsupportedSootUnitException(unit)
          }
          case i: DoubleType => unit.getOp2Type() match {
            case j: PrimType => unit.getUnder1Type() match{
              case k: PrimType => currprop.evalDup1_x2
              case k: Type => throw UnsupportedSootUnitException(unit)
            }
            case j: Type => throw UnsupportedSootUnitException(unit)
          }
          case i: PrimType => unit.getOp2Type() match {

            case j: PrimType => unit.getUnder1Type() match{
               case k: PrimType => unit.getUnder2Type() match{
                 case w: PrimType => currprop.evalDup2_x2
                 case w: Type => throw UnsupportedSootUnitException(unit)
               }
               case k: Type => throw UnsupportedSootUnitException(unit)
            }
            case j: Type => throw UnsupportedSootUnitException(unit)
          }
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: Dup2Inst =>
        unit.getOp1Type() match {
          case i: LongType => currprop.evalLocal(size-1)
          case i: DoubleType => currprop.evalLocal(size-1)
          case i: PrimType => unit.getOp2Type() match {
            case j: PrimType =>  currprop.evalDup2
            case j: Type =>  throw UnsupportedSootUnitException(unit)
          }
          case i: Type =>  throw UnsupportedSootUnitException(unit)
        }
      case unit: DynamicInvokeInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: EnterMonitorInst =>
        currprop.enterMonitor()
      case unit: ExitMonitorInst =>
        currprop.exitMonitor()
      case unit: FieldGetInst =>
        currprop.assignField(f=unit.getField())
      case unit: FieldPutInst =>
      	currprop.evalField(f=unit.getField())
      case unit: IdentityInst =>
        currprop
      case unit: IfNonNullInst =>
      	throw UnsupportedSootUnitException(unit) //Da fare
      case unit: IfNullInst =>
        throw UnsupportedSootUnitException(unit) //Da fare
      case unit: IncInst =>
        unit.getConstant() match {
          case i: IntConstant => currprop.evalInc(localMap(unit.getLocal), i.value)
          case i: LongConstant => currprop.evalInc(localMap(unit.getLocal), i.value)
          case i: FloatConstant => currprop.evalInc(localMap(unit.getLocal), i.value)
          case i: DoubleConstant => currprop.evalInc(localMap(unit.getLocal), i.value)
          case i: Constant => throw UnsupportedSootUnitException(unit)
        }
      case unit: InstanceCastInst =>
      	throw UnsupportedSootUnitException(unit)
      case unit: InstanceOfInst =>
      	throw UnsupportedSootUnitException(unit)
      case unit: InstSwitch =>
        //controllare
        throw UnsupportedSootUnitException(unit)
      case unit: InterfaceInvokeInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: LoadInst =>
        currprop.evalLocal(localMap(unit.getLocal))
      case unit: LookupSwitchInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: MulInst =>
        unit.getOpType() match {
          case i: PrimType => currprop.evalMul
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: NegInst =>
        unit.getOpType() match {
          case i: PrimType => currprop.evalNeg
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: NewArrayInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: NewInst =>
        currprop.evalNew(unit.getBaseType())
      case unit: NewMultiArrayInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: NopInst =>
      	currprop
      case unit: OrInst =>
        unit.getOpType() match{
          case i: PrimType => currprop.evalBinOp
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: PopInst =>
        //controllare
        currprop.restrict(unit.getWordCount())
      case unit: PrimitiveCastInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: PushInst =>
        unit.getConstant() match {
          case i: IntConstant=> currprop.evalConstant(i.value)
          case i: LongConstant => currprop.evalConstant(i.value)
          case i: FloatConstant => currprop.evalConstant(i.value)
          case i: DoubleConstant => currprop.evalConstant(i.value)
          case i: Constant => throw UnsupportedSootUnitException(unit)
        }
      case unit: RemInst =>
        unit.getOpType() match {
          case i: PrimType => currprop.evalRem
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: RetInst =>
      	exits :+= currprop
        currprop
      case unit: ReturnInst =>
        exits :+= currprop
        currprop
      case unit: ReturnVoidInst =>
        exits :+= currprop
        currprop
      case unit: ShlInst =>
        unit.getOpType() match{
          case i: PrimType => currprop.evalShl
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: ShrInst =>
        unit.getOpType() match{
          case i: PrimType => currprop.evalShr
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: SpecialInvokeInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: StaticGetInst =>
        throw UnsupportedSootUnitException(unit)
        //currprop.assignStaticField(f=unit.getFieldRef())
      case unit: StaticInvokeInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: StaticPutInst =>
        throw UnsupportedSootUnitException(unit)
        //currprop.evalStaticField(unit.getFieldRef())
      case unit: StoreInst =>
        currprop.assignLocal(localMap(unit.getLocal()))
      case unit: SubInst =>
        unit.getOpType() match {
          case i: PrimType => currprop.evalSub
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: SwapInst =>
        currprop.evalSwap()
      case unit: TableSwitchInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: TargetArgInst =>
        // le istruzioni if<cond> assumiamo che il valore di confronto sia intero
          val (tbranch, fbranch) = analyzeTargetArgInst(unit, currprop)
          exits :+= tbranch
          fbranch
      case unit: ThrowInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: UshrInst =>
        unit.getOpType() match{
          case i: PrimType => currprop.evalUshr
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: VirtualInvokeInst =>
        throw UnsupportedSootUnitException(unit)
      case unit: XorInst =>
        unit.getOpType() match{
          case i: PrimType => currprop.evalBinOp
          case i: Type => throw UnsupportedSootUnitException(unit)
        }
      case unit: Inst =>
        throw UnsupportedSootUnitException(unit)

      	/**
      	case unit: PushInst =>
          unit.getConstant() match {
            case i: IntConstant => currprop.evalConstant(i.value)
          }
        case unit: AddInst => currprop.evalAdd
        case unit: MulInst => currprop.evalMul
        case unit: SubInst => currprop.evalSub
        case unit: DivInst => currprop.evalDiv
        case unit: IncInst =>
          unit.getConstant() match {
            case i: IntConstant =>
              val l = localMap(unit.getLocal())
              // TODO: implement an inc method to speed up execution
              currprop.evalLocal(l).evalConstant(i.value).evalAdd.assignLocal(l)
          }
        case unit: StoreInst =>
          currprop.assignLocal(localMap(unit.getLocal))
        case unit: LoadInst =>
          currprop.evalLocal(localMap(unit.getLocal))
        case unit: TargetArgInst =>
          val (tbranch, fbranch) = analyzeTargetArgInst(unit, currprop)
          exits :+= tbranch
          fbranch
        case unit: ReturnVoidInst =>
          exits :+= currprop
          currprop
        case unit: Inst =>
          throw UnsupportedSootUnitException(unit)
        */
      }
    if (node.getTail.fallsThrough()) exits +:= currprop
    exits
  }
}

