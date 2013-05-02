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
class BafMethod(method: SootMethod) extends ControlFlowGraph[BafMethod,Unit] {
  import scala.collection.JavaConversions._

  type Node = Unit
  type DomainBase = JVMEnvDomain

  private val body = Baf.v().newBody(method.retrieveActiveBody())
  private val envMap = body.getLocals().zipWithIndex.toMap

  val graph = new ExceptionalUnitGraph(body)
  val size = body.getLocalCount()
  val ordering = new Ordering[Node] {
    val order = new PseudoTopologicalOrderer[Unit].newList(graph, false).zipWithIndex.toMap
    def compare(x: Node, y: Node) = order(x) - order(y)
  }

  /**
   * @note In developing this method we are assuming that, if a unit has a fall-through, it is the first
   * successor returned by `getSuccsOf`.
   */
  protected def analyzeBlock(params: Parameters)(node: Unit, prop: params.Property): Seq[params.Property] = {
    var exits = Seq[params.Property]()
    val fallProp = prop.clone

    def analyzeIf(op: AtomicCond.ComparisonOperators.Value) = {
      val jumpProp = prop.clone
      jumpProp.if_icmp(op)
      exits :+= jumpProp
      fallProp.if_icmp(AtomicCond.ComparisonOperators.opposite(op))
    }

    node match {
      case unit: PushInst =>
        unit.getConstant() match {
          case i: IntConstant => fallProp.ipush(i.value)
        }
      case unit: AddInst =>
        fallProp.iadd
      case unit: IncInst =>
        unit.getConstant() match {
          case i: IntConstant => fallProp.iinc(envMap(unit.getLocal), i.value)
        }
      case unit: StoreInst =>
        fallProp.istore(envMap(unit.getLocal))
      case unit: LoadInst =>
        fallProp.iload(envMap(unit.getLocal))
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
        exits :+= fallProp
      case unit: ReturnVoidInst =>
      case unit: Inst =>
        throw UnsupportedBafByteCodeException(unit)
    }
    if (node.fallsThrough) exits +:= fallProp
    exits
  }

  /**
   * Output the program intertwined with the given annotation. It uses the tag system of
   * Soot, but the result is manipulated heavily since we want tags to be printed before
   * the corresponding unit. It is an hack and may not work if there are comments in the
   * program.
   * @tparam D the type of the JVM environment used in the annotation.
   * @param ann the annotation to print together with the program.
   */
  def mkString[D <: JVMEnv[D]](ann: Annotation[ProgramPoint, D]): String = {
    // tag the method
    val localsList = body.getLocals().toIndexedSeq  map { _.getName() }
    for ((unit, prop) <- ann) {
      unit.addTag(new LoopInvariantTag("[ " + prop.mkString(localsList) + " ]"))
    }

    // generate output with tag
    Options.v().set_print_tags_in_output(true)
    val printer = Printer.v()
    val sw = new StringWriter()
    val ps = new PrintWriter(sw)
    printer.printTo(body, ps)
    ps.close()
    val out = sw.getBuffer.toString

    // mangle output to put annotations before the program code
    val lines = out.split("\n")
    for (i <- 0 until lines.length) {
      if (lines(i).startsWith("/*") && i > 0 && !lines(i - 1).startsWith("/*")) {
        val temp = lines(i)
        lines(i) = lines(i - 1)
        lines(i - 1) = temp
      }
    }

    // remove annotations
    for ((unit, prop) <- ann)
      unit.removeAllTags()
    lines.mkString("\n")
  }

  override def toString = mkString(getAnnotation[Null])
}

