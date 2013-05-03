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

package it.unich.sci.jandom.targets.cfg
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

import it.unich.sci.jandom.targets.Annotation
import it.unich.sci.jandom.targets.Target

import soot.toolkits.graph.DirectedGraph
import soot.util.Chain

/**
 * This is a generic analyzer for control flow graphs. It uses the `Soot` library and exploits F-bounded
 * polymorphism to ensure type safety.
 * @tparam Node the type of the nodes for the control flow graph.
 * @tparam Tgt the real class we are endowing with the ControlFlowGraph quality.
 * @author Gianluca Amato <gamato@unich.it>
 */
abstract class ControlFlowGraph[Tgt <: ControlFlowGraph[Tgt,Node],Node] extends Target[Tgt] {

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
   * This method is provided by subclasses, and should be able to analyze a single `Node`.
   * @param params the parameters of the analysis
   * @param node the node to analyze
   * @param prop the ingoing property to the node
   * @return a sequence of properties, one for each outgoing edge. The order of these properties should
   * correspond to the order of edges in `graph`.
   */
  protected def analyzeBlock(params: Parameters)(node: Node, prop: params.Property): Seq[params.Property]

  /**
   * The analyzer.  At the moment, it implements a work-list based analysis.
   */
  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property] = {
    import scala.collection.JavaConversions._

    val ann = getAnnotation[params.Property]
    val annEdge = HashMap[Edge, params.Property]()
    val taskList = Queue[ProgramPoint](graph.getHeads: _*)
    for (node <- graph.getHeads) ann(node) = params.domain.full(size)

    // ASCENDING phase
    params.log("Ascening Phase\n")
    while (!taskList.isEmpty) {
      val node = taskList.dequeue
      params.log(s"node ${node} input ${ann(node)} ")
      val result = analyzeBlock(params)(node, ann(node))
      params.log("result " + (graph.getSuccsOf(node) zip result).mkString(" ; ") + "\n")
      for ((succ, out) <- graph.getSuccsOf(node) zip result) {
        annEdge((node, succ)) = out
        if (graph.getPredsOf(succ).length>1 &&  (ann contains succ)) {
          params.log(s"join $succ : ${ann(succ)} with $out")
          val succval: params.Property = if (ordering.lteq(succ,node)) {
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
        val succval = if (ordering.lteq(succ,node)) {
          val narrowing = params.narrowingFactory(node)
          narrowing(ann(succ),newinput)
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
