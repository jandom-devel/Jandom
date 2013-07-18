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

import it.unich.sci.jandom.targets._

import soot.SootMethod
import soot.Scene
import soot.jimple.toolkits.callgraph.CHATransformer
import soot.jimple.toolkits.callgraph.Sources
import soot.jimple.toolkits.callgraph.TopologicalOrderer

/**
 * This is just a tag for interpretations which should be used in the analysis of Soot
 * methods. The input in a Soot interpretation has the following format:
 * - if the method is not static, the first dimension corresponds to `this`
 * - each further dimension corresponds to a parameter in the order of declaration
 * The output of a Soot interpretation has the same correspondence between dimensions
 * and signature, with the addition of a last parameters corresponding to the return type
 * if this is different from `void`.
 */
trait SootInterpretation[Tgt <: SootCFG[Tgt,_], Params <: Parameters[Tgt]] extends Interpretation[Tgt, Params] {

}
/**
 * A `TopSootInterpretation` always returns the top abstract element of the correct type for
 * each input.
 * @param params the parameters for the analysis
 */
class TopSootInterpretation[Tgt <: SootCFG[Tgt, _], Params <: Parameters[Tgt]](val params: Params) extends SootInterpretation[Tgt, Params] {
  def apply(method: SootMethod, input: params.Property): params.Property = params.domain.top(SootCFG.outputTypes(method))
}

/**
 * A `JimpleInterpretation` tries to return the semantics of a method by recursively analyzing its body.
 * It does not handle recursion, so it generates an exception if recursion is detected. It should only
 * be used for testing purposes. It only supports the target `JimpleMethod` for now.
 * @param params the parameters for the analysis
 * @throw IllegalArgumentException if recursive definitions are detected
 */
class JimpleInterpretation[Params <: Parameters[JimpleMethod]](val params: Params) extends SootInterpretation[JimpleMethod, Params] {
  /**
   * It maps each pair `(method, input)` with the pair `(output,rec)`. The first time a pair `(method, input)`
   * is encountered, the value `(bottom, false)` is put in `inte`. Then, the body of the method is analyzed.
   * If during the analysis the pair `(method, input)` is required again, execution stops with an exception,
   * otherwise `(output, true)` is saved into the map.
   */
  private val inte = scala.collection.mutable.HashMap[(SootMethod, params.Property), (params.Property, Boolean)]()
  private val jmethodCache = scala.collection.mutable.HashMap[SootMethod, JimpleMethod]()

  def apply(method: SootMethod, input: params.Property): params.Property = {
    if (inte contains (method, input)) inte(method, input) match {
      case (output, true) => output
      case (output, false) => throw new IllegalArgumentException("Recursive")
    }
    else {
      inte((method, input)) = (params.domain.bottom(SootCFG.outputTypes(method)), false)
      val jmethod = jmethodCache.getOrElseUpdate(method, new JimpleMethod(method))
      val ann = jmethod.analyzeFromInput(params)(input)
      val output = jmethod.extractOutput(params)(ann)
      inte((method, input)) = (output, true)
      output
    }
  }
}

/**
 * A `JimpleRecursiveInterpretation` tries to return the semantics of methods by a summary based analysis.
 * The semantics of all methods is initialized to top for every possible input, then methods are analyzed
 * with a work-list based approach. Each method has a single possible input context, which is top.
 */
class JimpleRecursiveInterpretation[Params <: Parameters[JimpleMethod]](scene: Scene, val params: Params) extends Interpretation[JimpleMethod, Params] {
  import scala.collection.JavaConversions._

  val inte = scala.collection.mutable.HashMap[SootMethod, params.Property]()
  val targets = scala.collection.mutable.HashMap[SootMethod, Option[JimpleMethod]]()

  def apply(method: SootMethod, input: params.Property): params.Property = {
    if (inte contains method)
      inte(method)
    else {
      val bottom = params.domain.bottom(SootCFG.outputTypes(method))
      inte(method) = bottom
      bottom
    }
  }

  def compute(method: SootMethod, input: params.Property) {
    val l = new java.util.LinkedList[SootMethod]()
    l.add(method)
    scene.setEntryPoints(l)
    CHATransformer.v().transform()
    val cg = scene.getCallGraph()
    val tpo = new TopologicalOrderer(cg)
    tpo.go()

    val order = tpo.order().reverse         // it is enough to get the set of all the elements
    if (order.isEmpty()) order.add(method)

    for (m <- order; if !(targets contains m)) {
      targets(m) = if (m.isConcrete()) Some(new JimpleMethod(m)) else None
      inte(m) = params.domain.bottom(SootCFG.outputTypes(m))
    }

    val worklist = scala.collection.mutable.Queue[SootMethod](order.toSeq: _*)
    while (!worklist.isEmpty) {
      val m = worklist.dequeue
      val jmethod = targets(m)
      val top = params.domain.top(SootCFG.inputTypes(m))
      val output = jmethod match {
        case None => inte(m)
        case Some(jmethod) => {
          val ann = jmethod.analyzeFromInput(params)(top)
          jmethod.extractOutput(params)(ann)
        }
      }
      if (!(inte(m) >= output)) {
        inte(m) = inte(m) widening output
        val sources = new Sources(cg.edgesInto(m)).asInstanceOf[java.util.Iterator[SootMethod]]
        worklist.enqueue(sources.toSeq: _*)
      }
    }
  }

  override def toString = inte.toString
}
