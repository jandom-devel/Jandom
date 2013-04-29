/**
 * Copyright 2013 amato
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
 * @author Gianluca Amato
 */
abstract class ControlFlowGraph extends Target {
  type Node
  type Edge = (Node, Node)
  type ProgramPoint = Node
  type Tgt <: ControlFlowGraph

  val chain: Chain[Node]
  val graph: DirectedGraph[Node]
  val order: Annotation[Node, Int]
  val size: Int

  protected def analyzeBlock(params: Parameters)(node: Node, prop: params.Property): Seq[params.Property]

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
        if (ann contains succ) {
          params.log(s"join $succ : ${ann(succ)} with $out")
          val succval = if (order(succ) <= order(node))
            ann(succ) widening out
          else
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
    taskList.enqueue(chain.iterator.toSeq :_*)
    params.log("Descending Phase\n")
    while (!taskList.isEmpty) {
      val node = taskList.dequeue
      params.log(s"node ${node} input ${ann(node)} ")
      val result = analyzeBlock(params)(node, ann(node))
      params.log("result " + (graph.getSuccsOf(node) zip result).mkString(" ; ") + "\n")
      for ((succ, out) <- graph.getSuccsOf(node) zip result) {
        annEdge((node, succ)) = out
        val newinput = graph.getPredsOf(succ) map { annEdge(_,succ) } reduce { _ union _ }
        params.log(s"narrow $succ : ${ann(succ)} with $newinput ")
        // this may probably cause an infinite loop
        val succval = if (order(succ) <= order(node))
            ann(succ) narrowing newinput
          else
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
