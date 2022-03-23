/**
  * Copyright 2013, 2018 Gianluca Amato <gianluca.amato@unich.it>, Francesca Scozzari <fscozzari@unich.it>
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

package it.unich.jandom.targets.jvmsoot

import java.io._

import scala.jdk.CollectionConverters._
import it.unich.jandom.targets.Annotation
import it.unich.jandom.targets.Environment
import it.unich.jandom.targets.cfg.ControlFlowGraph

import soot._
import soot.options.Options
import soot.tagkit.LoopInvariantTag
import soot.toolkits.graph.Block
import soot.toolkits.graph.PseudoTopologicalOrderer

import scala.collection.immutable

/**
  * This class is an ancestor for all the analyzers of JVM methods using the Soot library.
  *
  * @tparam Tgt  the real class we are endowing with the ControlFlowGraph quality.
  * @tparam Node the type of the nodes for the control flow graph.
  * @param method the method we want to analyze
  * @param io     whether I/O semantics or standard semantics is desired. It should agree with the value of `params.io` in
  *               methods which requires parameters.
  * @author Gianluca Amato <gamato@unich.it>
  * @author Francesca Scozzari <fscozzari@unich.it>
  */
abstract class SootCFG[Tgt <: SootCFG[Tgt, Node], Node <: Block](val method: SootMethod, io: Boolean) extends ControlFlowGraph[Tgt, Node] {
  type DomainBase = SootFrameDomain

  val body: Body

  // local variables of the method.
  def locals: immutable.IndexedSeq[Local] = body.getLocals.asScala.toIndexedSeq

  // These are lazy values since body is not initialized when they are executed

  // The last program point. If there is more then one tail in the graph, this returns the first tail.
  lazy val lastPP = Some(graph.getTails.get(0))

  // The size number of local variables in the method.
  //lazy val size = body.getLocalCount()

  // An ordering on nodes gives by a pseudo-topological orderer
  lazy val ordering: Ordering[Node] = new Ordering[Node] {
    val order: Map[Node, Int] = new PseudoTopologicalOrderer[Node].newList(graph, false).asScala.zipWithIndex.toMap

    def compare(x: Node, y: Node): Int = order(x) - order(y)
  }

  /**
    * A map from a local to its dimension. The first inputTypes dimensions are for input parameters when the IO
    * semantics is enabled, then come local variables.
    */
  lazy val localMap: Map[Local, Int] = if (io)
    locals.zipWithIndex.map { case (l, i) => (l, i + inputTypes.length) }.toMap
  else
    locals.zipWithIndex.toMap

  /**
    * Returns the sequence of types for parameters and locals. It is the opposite of localMap (although localMap
    * has no entry for parameters, only for locals). Every program state has a number of dimensions equal to
    * the number of element of localTypes.
    */
  lazy val localTypes: Seq[Type] = (if (io) inputTypes else Seq()) ++ (locals map (_.getType()))

  /**
    * The size of the environment within the method analyzer.
    */
  lazy val size: Int = localTypes.size

  /**
    * Returns the sequence of types required as input for every interpretation of this CFG.
    */
  def inputTypes: Seq[Type] = SootCFG.inputTypes(method)

  /**
    * Returns the sequence of types to be returned by every interpretation of this CFG.
    */
  def outputTypes: Seq[Type] = SootCFG.outputTypes(method)

  /**
    * Returns an environment which may be used to parse an input property of a SootMethod
    */
  def inputEnvironment: Environment = {
    val env = Environment()
    if (!method.isStatic)
      env.addBinding("@this")
    for (i <- 0 until method.getParameterCount)
      env.addBinding("@parameter" + i)
    env
  }

  /**
    * Returns an environment which may be used to parse an internal property of a SootMethod
    */
  def environment: Environment = {
    val env = inputEnvironment
    for (l <- locals)
      env.addBinding(l.getName)
    env
  }

  /**
    * Returns an environment which may be used to parse an output property of a SootMethod
    */
  def outputEnvironment: Environment = {
    val env = environment
    if (method.getReturnType != VoidType.v())
      env.addBinding("@return")
    env
  }

  /**
    * @inheritdoc
    * It expands the input property adding new variables until exhausting locals.
    */
  protected def expandPropertyWithLocalVariables(params: Parameters)(input: params.Property): params.Property = {
    body.getLocals.asScala.foldLeft(input) { (current, local) => current.evalUnknown(local.getType) }
  }

  /**
    * @inheritdoc
    * Extracts the output at the tails nodes and restrict to the only dimensions relevant for
    * the semantics of the method.
    */
  override def extractOutput(params: Parameters)(ann: Annotation[ProgramPoint, params.Property]): params.Property = {
    // we extract the result of the analysis
    val output = super.extractOutput(params)(ann)
    if (io) {
      // we remove the copy of the parameters (local variables)
      method.getReturnType match {
        case _: VoidType => output.delVariables(SootCFG.inputTypes(method).size until output.dimension)
        case _ => output.delVariables(SootCFG.inputTypes(method).size until output.dimension - 1)
      }
    } else
    // for intra-procedural analysis we return all the local variables plus the returned value in the last position
      output
  }

  protected def topProperty(node: Node, params: Parameters): params.Property = {
    if (io)
      expandPropertyWithLocalVariables(params)(params.domain.top(inputTypes))
    else
      params.domain.top(method.getActiveBody.getLocals.asScala.toSeq map (_.getType))
  }

  def formatProperty(params: Parameters, lastPP: Boolean = false)(prop: params.Property): String = {
    val localNames = locals map (_.getName)
    // we denote by "@return" the return value of the method
    val returnValue = if (method.getReturnType != VoidType.v()) Seq("@return") else Seq()
    val thisVariable = if (!method.isStatic) Seq("@this") else Seq()
    val parameterNames = for (i <- 0 until method.getParameterCount) yield "@parameter" + i
    if (io) {
      if (lastPP)
        prop.mkString(thisVariable ++ parameterNames ++ returnValue)
      else
        prop.mkString(thisVariable ++ parameterNames ++ localNames)
    } else {
      prop.mkString(localNames ++ returnValue)
    }
  }

  /**
    * Output the program intertwined with the given annotation. It uses the tag system of
    * Soot, but the result is manipulated heavily since we want tags to be printed before
    * the corresponding unit. It is an hack and may not work if there are comments in the
    * program.
    *
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
    for ((node, _) <- ann; unit = node.getHead; if unit != null)
      unit.removeAllTags()

    // in the last program point we also show the return value (@return)
    val outLine = if (ann contains lastPP.get) {
      if (io)
        "/* Output: " + formatProperty(params, lastPP = true)(extractOutput(params)(ann)) + " */\n"
      else
        "/* Output: " + formatProperty(params, lastPP = true)(ann(lastPP.get)) + " */\n"
    } else
      ""
    lines.mkString("", "\n", "\n") + outLine
  }

  /**
    * @inheritdoc
    * The default implementation just reuse `mkString` with an empty `Null` annotation.
    */
  override def toString: String = body.toString
}

/**
  * This is an helper object providing some helper methods on Soot. This is only a temporary workaround,
  * and should be removed in the future.
  */
object SootCFG {
  /**
    * Returns the sequence of types to be returned by every interpretation of a SootMethod
    * The sequence of variable types is:
    *  - type of @this (only if the method is not static)
    *  - types of parameters @parameter0, @parameter1, ...
    *  - type of return value (only if the return type is not void)
    */
  def outputTypes(method: SootMethod): Seq[Type] = {
    val returnTypes =
      if (method.getReturnType == VoidType.v())
        Seq()
      else
        Seq(method.getReturnType)
    inputTypes(method) ++ returnTypes
  }

  /**
    * Returns the sequence of types required as input for a SootMethod
    * The sequence of variable types is:
    *  - type of @this (only if the method is not static)
    *  - types of parameters @parameter0, @parameter1, ...
    */
  def inputTypes(method: SootMethod): Seq[Type] = {
    val thisType =
      if (method.isStatic)
        Seq()
      else
        Seq(method.getDeclaringClass.getType)
    val parameterTypes = method.getParameterTypes.asScala
    thisType ++ parameterTypes
  }
}
