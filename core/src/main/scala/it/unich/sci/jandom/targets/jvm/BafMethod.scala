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
 * This class analyzes a method of a Java class. It uses the Baf intermediate representation of the Soot library.
 * @author Gianluca Amato
 */
class BafMethod(method: SootMethod) extends ControlFlowGraph {
  import scala.collection.JavaConversions._

  type Node = Unit
  type Tgt = BafMethod
  type DomainBase = JVMEnvDomain

  val jimple = method.retrieveActiveBody()
  val body = Baf.v().newBody(jimple)
  val chain = body.getUnits()
  val graph = new ExceptionalUnitGraph(body)
  val order = weakTopologicalOrder

  val envMap = new HashMap[Local, Int]
  val locals = body.getLocals()
  val localsList = locals.iterator.toArray map { _.getName() }
  val i = locals.iterator
  for (n <- 0 until locals.size) {
    envMap(i.next()) = n
  }
  val size = locals.size

  lazy val weakTopologicalOrder: Annotation[Unit, Int] = {
    val order = new PseudoTopologicalOrderer[Unit].newList(graph, false)
    val ann = getAnnotation[Int]
    var index = 0
    order.iterator.foreach { u => ann(u) = index; index += 1 }
    ann
  }

  protected def analyzeBlock(params: Parameters)(node: Unit, prop: params.Property): Seq[params.Property] = {
    var exits = Seq[params.Property]()
    val newprop = prop.clone

    def analyzeIf(op: AtomicCond.ComparisonOperators.Value) = {
      val scopy = prop.clone
      scopy.if_icmp(op)
      exits :+= scopy
      newprop.if_icmp(AtomicCond.ComparisonOperators.opposite(op))
    }

    node match {
      case unit: PushInst =>
        unit.getConstant() match {
          case i: IntConstant => newprop.ipush(i.value)
        }
      case unit: AddInst =>
        newprop.iadd
      case unit: IncInst =>
        unit.getConstant() match {
          case i: IntConstant => newprop.iinc(envMap(unit.getLocal), i.value)
        }
      case unit: StoreInst =>
        newprop.istore(envMap(unit.getLocal))
      case unit: LoadInst =>
        newprop.iload(envMap(unit.getLocal))
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
        exits :+= newprop
      case unit: ReturnVoidInst =>
      case unit: Inst =>
        throw UnsupportedBafByteCodeException(unit)
    }
    if (node.fallsThrough) exits +:= newprop
    exits
  }

  def mkString[D <: JVMEnv[D]](ann: Annotation[ProgramPoint, D]): String = {
    for ((unit, prop) <- ann) {
      unit.addTag(new LoopInvariantTag("[ " + prop.mkString(localsList) + " ]"))
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

