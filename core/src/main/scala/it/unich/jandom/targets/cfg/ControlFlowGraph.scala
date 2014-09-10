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

package it.unich.jandom.targets.cfg
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

import it.unich.jandom.targets.Annotation
import it.unich.jandom.targets.Target

import soot.toolkits.graph.DirectedGraph
import soot.util.Chain

/**
 * This is a generic analyzer for control flow graphs. It uses the `Soot` library and exploits F-bounded
 * polymorphism to ensure type safety.
 * @tparam Node the type of the nodes for the control flow graph.
 * @tparam Tgt the real class we are endowing with the ControlFlowGraph quality.
 * @author Gianluca Amato <gamato@unich.it>
 */
abstract class ControlFlowGraph[Tgt <: ControlFlowGraph[Tgt, Node], Node] extends Target[Tgt] {
  import scala.collection.JavaConversions._

  /**
   * The `ProgramPoint` type is defined as an alias for the `Node`. We are wondering whether to make
   * `ProgramPoint` an alias for `Edge`.
   */
  type ProgramPoint = Node

  /**
   * The analyzer needs a representation of the CFG. Here we use the class `DirectedGraph` of `Soot`.
   */
  val graph: DirectedGraph[Node]

  /**
   * In order to determine widening points, we need an ordering on nodes.
   */
  val ordering: Ordering[Node]

  /**
   * The dimension of the environment space. It is used to get the initial values of the abstract domains.
   * It should be removed once we have methods `empty` and `full` which do not depend on a given dimension.
   */
  val size: Int

  private type Edge = (Node, Node)

  /**
   * Returns the output property embedded in an annotation. It essentially consider the union of the result of analyzing
   * the tail nodes of the directed graph starting from their annotation.
   * @param params the parameters used to generate the annotation
   * @param ann the annotation
   * @note The implementation is not very robust, since we are assuming a lot of thing on the result of analyzing
   * a tail block. In particular, we are assuming that the last result of analyzeBlock is the "output"
   * of the CFG, so be careful to preserve this property.
   */
  def extractOutput(params: Parameters)(ann: Annotation[ProgramPoint, params.Property]): params.Property =
    graph.getTails map { (node) => analyzeBlock(params)(node, ann(node)).last } reduce { _ union _ }

  /**
   * This method adapt an input property (expressed typically only in terms of the input
   * parameters) in a new property with additional information needed to carry on the analysis.
   */
  protected def adaptProperty(params: Parameters)(input: params.Property): params.Property

  /**
   * This method returns the top property for a given node in the CFG
   * @param node the node for which the top element should be determined
   * @param params parameters of the analysis
   */
  protected def topProperty(node: Node, params: Parameters): params.Property

  /**
   * This method is provided by subclasses, and should be able to analyze a single `Node`.
   * @param params the parameters of the analysis
   * @param node the node to analyze
   * @param prop the ingoing property to the node
   * @return a sequence of properties, one for each outgoing edge. The order of these properties should
   * correspond to the order of edges in `graph`. For the tails, the last element should be the value
   * returned as the result of the CFG.
   */
  protected def analyzeBlock(params: Parameters)(node: Node, prop: params.Property): Seq[params.Property]

  /**
   * Analyzes the target, starting from a given property.
   * @param param the parameters which drive the analyzer
   * @param input the starting property
   * @return the resulting annotation
   * @note this should be moved in the Target class.
   */
  def analyzeFromInput(params: Parameters)(input: params.Property): Annotation[ProgramPoint, params.Property] = {
    val ann = getAnnotation[params.Property]
    for (node <- graph.getHeads()) ann(node) = adaptProperty(params)(input)
    analyzeFromAnnotation(params)(ann)
  }

  /**
   * Perform a static analysis over the target, from a standard initial annotation
   * @param param the parameters which drive the analyzer
   * @return an annotation for the program
   */
  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property] = {
    val ann = getAnnotation[params.Property]
    for (node <- graph.getHeads) ann(node) = topProperty(node, params)
    analyzeFromAnnotation(params)(ann)
  }

  /**
   * The analyzer. At the moment, it implements a work-list based analysis.
   */
  def analyzeFromAnnotation(params: Parameters)(ann: Annotation[ProgramPoint, params.Property]): Annotation[ProgramPoint, params.Property] = {
    val annEdge = HashMap[Edge, params.Property]()
    val taskList = Queue[ProgramPoint](graph.getHeads: _*)

    // ASCENDING phase
    params.log("Ascening Phase\n")
    while (!taskList.isEmpty) {
      val node = taskList.dequeue
      params.log(s"node ${node}input ${ann(node)}\n")
      val result = analyzeBlock(params)(node, ann(node))
      params.log("result " + result.mkString(",") + "\n")
      for ((succ, out) <- graph.getSuccsOf(node) zip result) {
        annEdge((node, succ)) = out
        if (graph.getPredsOf(succ).length > 1 && (ann contains succ)) {
          params.log(s"join $succ : ${ann(succ)} with $out")
          val succval: params.Property = if (ordering.lteq(succ, node)) {
            params.log(s" widening")
            val widening = params.wideningFactory(node)
            widening(ann(succ), out)
          } else
            ann(succ) union out
          if (succval > ann(succ)) {
            params.log(s" update with $succval\n")
            ann(succ) = succval
            taskList.enqueue(succ)
          } else {
            params.log(s" not updated\n")
          }
        } else {
          ann(succ) = out
          taskList.enqueue(succ)
        }
      }
    }

    // DESCENDING phase
    taskList.enqueue(graph.toSeq: _*)
    params.log("Descending Phase\n")
    while (!taskList.isEmpty) {
      val node = taskList.dequeue
      params.log(s"node ${node} input ${ann(node)} ")
      val result = analyzeBlock(params)(node, ann(node))
      params.log("result " + (graph.getSuccsOf(node) zip result).mkString(" ; ") + "\n")
      for ((succ, out) <- graph.getSuccsOf(node) zip result) {
        annEdge((node, succ)) = out
        val newinput = graph.getPredsOf(succ) map { annEdge(_, succ) } reduce { _ union _ }
        params.log(s"narrow $succ : ${ann(succ)} with $newinput ")
        // this may probably cause an infinite loop
        val succval = if (ordering.lteq(succ, node)) {
          val narrowing = params.narrowingFactory(node)
          narrowing(ann(succ), newinput)
        } else
          newinput
        params.log(s"result $succval\n")
        if (succval < ann(succ)) {
          ann(succ) = succval
          taskList.enqueue(succ)
        }
      }
    }
    ann
  }
}
