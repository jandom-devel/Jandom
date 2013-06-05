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

import soot._
import it.unich.sci.jandom.targets.Parameters
import scala.collection.mutable.HashMap
import it.unich.sci.jandom.targets.Target
import soot.jimple.toolkits.callgraph.CHATransformer
import soot.jimple.toolkits.callgraph.TopologicalOrderer
import scala.collection.mutable.Queue
import soot.jimple.toolkits.callgraph.Sources

/**
 * @author Gianluca Amato <gamato@unich.it>
 *
 */

trait Interpretation[Tgt <: Target[Tgt], Params <: Parameters[Tgt]] {
  val params: Params
  def apply(method: SootMethod, input: params.Property): params.Property
}

class TopSootInterpretation[Tgt <: SootCFG[Tgt, _], Params <: Parameters[Tgt]](val params: Params)  extends Interpretation[Tgt, Params] {
  import scala.collection.JavaConversions._
  def apply(method: SootMethod, input: params.Property): params.Property =
    if (method.getReturnType() == VoidType.v())
      params.domain.top(method.getParameterTypes().toSeq.asInstanceOf[Seq[Type]])
    else
      params.domain.top(method.getReturnType() +: method.getParameterTypes().toSeq.asInstanceOf[Seq[Type]])
}

// this only works for non recursive calls
class JimpleInterpretation[Params <: Parameters[JimpleMethod]](val params: Params) extends Interpretation[JimpleMethod, Params] {
  import scala.collection.JavaConversions._

  val inte = scala.collection.mutable.HashMap[(SootMethod, params.Property), (params.Property, Boolean)]()

  def apply(method: SootMethod, input: params.Property): params.Property = {
    if (inte contains (method, input)) inte(method, input) match {
      case (output, true) => output
      case (output, false) => throw new IllegalArgumentException("Recursive")
    }
    else {
      val jmethod = new JimpleMethod(method)
      val outFibers = method.getParameterTypes().asInstanceOf[java.util.List[Type]] :+ method.getReturnType()
      inte((method, input)) = (params.domain.bottom(outFibers), false)
      val ann = jmethod.analyzeFromInput(params)(input)
      val output = jmethod.extractOutput(params)(ann)
      inte((method, input)) = (output, true)
      output
    }
  }
}

class JimpleRecursiveInterpretation[Params <: Parameters[JimpleMethod]](scene: Scene, val params: Params) extends Interpretation[JimpleMethod, Params] {
  import scala.collection.JavaConversions._

  val inte = scala.collection.mutable.HashMap[SootMethod, params.Property]()
  val targets: scala.collection.mutable.HashMap[SootMethod, Option[JimpleMethod]] = scala.collection.mutable.HashMap()

  def apply(method: SootMethod, input: params.Property): params.Property = {
    if (inte contains method)
      inte(method)
    else {
      val bottom = params.domain.bottom(method.getReturnType() +: method.getParameterTypes().toSeq.asInstanceOf[Seq[Type]])
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

    val order = tpo.order().reverse // it is enough to get the set of all the elements
    if (order.isEmpty()) order.add(method)

    for (m <- order; if !(targets contains m)) {
      targets(m) = if (m.isConcrete()) Some(new JimpleMethod(m)) else None
      val resultType = if (m.getReturnType() == VoidType.v()) Seq() else Seq(m.getReturnType())
      inte(m) = params.domain.bottom(resultType ++ m.getParameterTypes().asInstanceOf[java.util.List[Type]])
    }

    val worklist = scala.collection.mutable.Queue[SootMethod](order.toSeq: _*)
    while (!worklist.isEmpty) {
      val m = worklist.dequeue
      val jmethod = targets(m)
      val top = params.domain.top(m.getParameterTypes().toSeq.asInstanceOf[Seq[Type]])
      val output = jmethod match {
        case None => inte(m)
        case Some(jmethod) => {
          val ann = jmethod.analyzeFromInput(params)(top)
          jmethod.extractOutput(params)(ann)
        }
      }
      if (!(inte(m) >= output)) {
        println(inte(m),output)
        inte(m) = inte(m) widening output
        val sources = new Sources(cg.edgesInto(m)).asInstanceOf[java.util.Iterator[SootMethod]]
        worklist.enqueue(sources.toSeq: _*)
      }
    }
  }

  override def toString = inte.toString

}
