/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

import java.io._

import it.unich.sci.jandom.domains.AbstractProperty
import it.unich.sci.jandom.targets.Annotation
import it.unich.sci.jandom.targets.cfg.ControlFlowGraph

import soot._
import soot.options.Options
import soot.tagkit.LoopInvariantTag
import soot.toolkits.graph.Block
import soot.toolkits.graph.PseudoTopologicalOrderer

/**
 * This class is an ancestor for all the analyzers of JVM methods using the Soot library.
 * @tparam Node the type of the nodes for the control flow graph.
 * @tparam Tgt the real class we are endowing with the ControlFlowGraph quality.
 * @param method the method we want to analyze
 * @author Gianluca Amato <gamato@unich.it>
 */
abstract class SootCFG[Tgt <: SootCFG[Tgt, Node], Node <: Block](val method: SootMethod) extends ControlFlowGraph[Tgt, Node] {
  import scala.collection.JavaConversions._

  type DomainBase = SootFrameDomain

  val body: Body
  def locals = body.getLocals().toIndexedSeq

  // These are lazy values since body is not initialized when they are executed
  lazy val lastPP = Some(graph.getTails().get(0))
  lazy val size = body.getLocalCount()
  lazy val ordering = new Ordering[Node] {
    val order = new PseudoTopologicalOrderer[Node].newList(graph, false).zipWithIndex.toMap
    def compare(x: Node, y: Node) = order(x) - order(y)
  }
  lazy val localMap = locals.zipWithIndex.toMap

  /**
   * @inheritdoc
   * At the moment, I am assuming that the first locals are exactly the parameters
   * of the method. It essentially expands the input property adding new variables
   * until exhausting locals. If the input/output analysis is active, it duplicates the
   * input parameters.
   */
  protected def adaptProperty(params: Parameters)(input: params.Property): params.Property = {
    assert(input.size <= body.getLocalCount(), s"Actual parameters <${input}> to method ${method} are more than the formal parameters")
    var currprop = input
    for (i <- input.size until body.getLocalCount()) currprop = currprop.evalNew(locals(i).getType())
    if (params.io) {
      for (i <- 0 until SootCFG.inputTypes(method).size) currprop = currprop.evalLocal(i)
    }
    currprop
  }

  /**
   * @inheritdoc
   * Extracts the output at the tails nodes and restrict to the only dimensions relevant for
   * the semantics of the method.
   */
  override def extractOutput(params: Parameters)(ann: Annotation[ProgramPoint, params.Property]): params.Property = {
    val tmp = super.extractOutput(params)(ann)
    if (params.io)
      tmp.restrict(SootCFG.outputTypes(method).size)
    else
      throw new IllegalArgumentException("Only supported with I/O semantics")
  }

  protected def topProperty(node: Node, params: Parameters): params.Property = {
    val inputParams = SootCFG.inputTypes(method)
    adaptProperty(params)(params.domain.top(inputParams))
  }

  def formatProperty(params: Parameters)(prop: params.Property) = {
    val localNames = locals map { _.getName() }
    val parameterNames = if (params.io) (for (i <- 0 until method.getParameterCount()) yield "@p" + i) else Seq()
    val stackPositions = prop.size - body.getLocalCount() - (if (params.io) method.getParameterCount() else 0)
    val stackNames = for (i <- 0 until stackPositions) yield "#s" + i
    val names = localNames ++ parameterNames ++ stackNames
    prop.mkString(names)
  }

  /**
   * Output the program intertwined with the given annotation. It uses the tag system of
   * Soot, but the result is manipulated heavily since we want tags to be printed before
   * the corresponding unit. It is an hack and may not work if there are comments in the
   * program.
   * @tparam D the type of the JVM environment used in the annotation.
   * @param ann the annotation to print together with the program.
   */
  def mkString(params: Parameters)(ann: Annotation[ProgramPoint, params.Property]): String = {

    // tag the method
    for ((node, prop) <- ann; unit = node.getHead; if unit != null)
      unit.addTag(new LoopInvariantTag("[ " + formatProperty(params)(prop) + " ]"))

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
    for ((node, prop) <- ann; unit = node.getHead; if unit != null)
      unit.removeAllTags()

    val outLine = if (ann contains lastPP.get) {
      "/* Output: " + formatProperty(params)(ann(lastPP.get)) + " */\n"
    } else
      ""
    lines.mkString("", "\n", "\n") + outLine
  }

  /**
   * @inheritdoc
   * The default implementation just reuse `mkString` with an empty `Null` annotation.
   */
  override def toString = body.toString
}

/**
 * This is an helper object providing some helper methods on Soot. This is only a temporary workaround,
 * and should be removed in the future.
 */
object SootCFG {
  /**
   * Returns the sequence of types to be returned by every interpretation of a SootMethod
   */
  def outputTypes(method: SootMethod) = {
    import scala.collection.JavaConversions._
    val returnTypes =
      if (method.getReturnType() == VoidType.v())
        Seq()
      else
        Seq(method.getReturnType())
    inputTypes(method) ++ returnTypes
  }

  /**
   * Returns the sequence of types required as input for a SootMethod
   */
  def inputTypes(method: SootMethod) = {
    import scala.collection.JavaConversions._
    val parameterTypes = method.getParameterTypes().toSeq.asInstanceOf[Seq[Type]]
    val thisTypes =
      if (method.isStatic())
        Seq()
      else
        Seq(method.getDeclaringClass().getType())
    thisTypes ++ parameterTypes
  }
}
