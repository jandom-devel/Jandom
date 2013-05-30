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
      case _: IfCmpLtInst => prop.testLt
      case _: IfCmpGtInst => prop.testGt
      case _: IfCmpLeInst => prop.testLe
      case _: IfCmpGeInst => prop.testGe
      case _: IfCmpEqInst => prop.testEq
      case _: IfCmpNeInst => prop.testNe
    }

    for (unit <- node.iterator())
      currprop = unit match {
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
      }
    if (node.getTail.fallsThrough()) exits +:= currprop
    exits
  }
}

