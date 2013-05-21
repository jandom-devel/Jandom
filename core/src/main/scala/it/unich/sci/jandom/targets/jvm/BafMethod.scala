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

package it.unich.sci.jandom.targets.jvm

import java.io.PrintWriter
import java.io.StringWriter

import scala.Array.canBuildFrom
import scala.collection.mutable.HashMap

import it.unich.sci.jandom.targets.Annotation
import it.unich.sci.jandom.targets.cfg.ControlFlowGraph
import it.unich.sci.jandom.targets.linearcondition.AtomicCond

import soot._
import soot.baf._
import soot.jimple._
import soot.options.Options
import soot.tagkit.LoopInvariantTag
import soot.toolkits.graph._

/**
 * This class analyzes a method of a Java class. It uses the Baf intermediate representation of the Soot library.  It is
 * based on the generic analyzer for control flow graphs.
 * @param method the method we want to analyze
 * @author Gianluca Amato
 */
class BafMethod(method: SootMethod) extends SootCFG[BafMethod,Block] {
  import scala.collection.JavaConversions._

  type DomainBase = JVMEnvDomain

  val body = Baf.v().newBody(method.retrieveActiveBody())
  val graph = new ExceptionalBlockGraph(body)
  BlockGraphConverter.addStartStopNodesTo(graph)

  private val envMap = body.getLocals().zipWithIndex.toMap

  def topProperty(node: Block, params: Parameters): params.Property = params.domain.full(body.getLocalCount())

  /**
   * @note In developing this method we are assuming that, if a unit has a fall-through, it is the first
   * successor returned by `getSuccsOf`.
   */
  protected def analyzeBlock(params: Parameters)(node: Block, initprop: params.Property): Seq[params.Property] = {
    var exits = Seq[params.Property]()
    var currprop = initprop.clone

    def analyzeIf(op: AtomicCond.ComparisonOperators.Value) = {
      val jumpProp = currprop.clone
      jumpProp.if_icmp(op)
      exits :+= jumpProp
      currprop.if_icmp(AtomicCond.ComparisonOperators.opposite(op))
    }

    for (unit <- node.iterator()) unit match {
      case unit: PushInst =>
        unit.getConstant() match {
          case i: IntConstant => currprop.ipush(i.value)
        }
      case unit: AddInst =>
        currprop.iadd
      case unit: IncInst =>
        unit.getConstant() match {
          case i: IntConstant => currprop.iinc(envMap(unit.getLocal), i.value)
        }
      case unit: StoreInst =>
        currprop.istore(envMap(unit.getLocal))
      case unit: LoadInst =>
        currprop.iload(envMap(unit.getLocal))
      case unit: IfCmpLeInst =>
        analyzeIf(AtomicCond.ComparisonOperators.LTE)
      case unit: IfCmpLtInst =>
        analyzeIf(AtomicCond.ComparisonOperators.LT)
      case unit: IfCmpGeInst =>
        analyzeIf(AtomicCond.ComparisonOperators.GTE)
      case unit: IfCmpGtInst =>
        analyzeIf(AtomicCond.ComparisonOperators.GT)
      case unit: IfCmpEqInst =>
        analyzeIf(AtomicCond.ComparisonOperators.EQ)
      case unit: IfCmpNeInst =>
        analyzeIf(AtomicCond.ComparisonOperators.NEQ)
      case unit: GotoInst =>
        exits :+= currprop
      case unit: ReturnVoidInst =>
        exits :+= currprop
      case unit: Inst =>
        throw UnsupportedBafByteCodeException(unit)
    }
    if (node.getTail==null || node.getTail.fallsThrough()) exits +:= currprop
    exits
  }
}

