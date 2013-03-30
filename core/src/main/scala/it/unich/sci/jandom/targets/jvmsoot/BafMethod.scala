/**
 * Copyright 2013 Gianluca Amato
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

import soot.baf._
import soot.jimple._
import soot._
import it.unich.sci.jandom.targets.Target
import scala.collection.mutable.HashMap
import it.unich.sci.jandom.targets.jvm.JVMEnvDomain
import java.io.StringWriter
import java.io.PrintWriter
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.toolkits.graph.ExceptionalBlockGraph
import soot.toolkits.graph.PseudoTopologicalOrderer
import soot.toolkits.graph.Block
import scala.collection.mutable.Queue
import it.unich.sci.jandom.targets.jvm.JVMEnv
import it.unich.sci.jandom.domains.NumericalProperty
import soot.tagkit.StringTag
import soot.options.Options

/**
 * Analysis of a method using the Baf intermediate representation.
 * @author Gianluca Amato
 *
 */
class BafMethod(method: SootMethod) extends Target {
  type ProgramPoint = Unit
  type Annotation[Property] = HashMap[ProgramPoint, Property]
  type Tgt = BafMethod
  type DomainBase = JVMEnvDomain

  val jimple = method.retrieveActiveBody()
  val body = Baf.v().newBody(jimple)
  val cfg = new ExceptionalUnitGraph(body)
  val order = new PseudoTopologicalOrderer[Unit].newList(cfg, false)

  def getAnnotation[Property]: Annotation[Property] = new Annotation[Property]

  def analyzePP[Property <: NumericalProperty[Property]](pp: ProgramPoint, ann: Annotation[JVMEnv[Property]]): Seq[(ProgramPoint, JVMEnv[Property])] = {
    import scala.collection.JavaConversions._

    var exits = Seq[(ProgramPoint, JVMEnv[Property])]()
    val state = ann(pp).clone
    var unit = pp
    var nextunit = pp
    do {
      unit = nextunit      
      unit match {
        case unit: PushInst =>
          unit.getConstant() match {
            case i: IntConstant => state.ipush(i.value)
          }
        case unit: AddInst =>
          state.iadd
        case unit: StoreInst =>
          state.istore(unit.getLocal.getNumber)
        case unit: ReturnVoidInst =>
          ann(unit) = state          
      }      
      if (!unit.branches && unit.fallsThrough())
        nextunit = cfg.getSuccsOf(unit).get(0)
    } while (!unit.branches() && unit.fallsThrough())
    if (unit.fallsThrough)
      exits :+= ((cfg.getSuccsOf(pp).get(0), state))
    exits
  }

  def analyze(params: Parameters): Annotation[params.Property] = {
    import scala.collection.JavaConversions._

    val ann = new Annotation[params.Property]()
    val taskList = Queue[ProgramPoint](cfg.getHeads(): _*)
    for (pp <- cfg.getHeads())
      ann(pp) = params.domain.full(body.getLocalCount())
    while (!taskList.isEmpty) {
      val pp = taskList.dequeue()
      val result = analyzePP(pp, ann)
      for ((destpp, state) <- result) {
        if (ann contains destpp) {
          val modified = if (false)
            ann(destpp).widening(state)
          else
            ann(destpp).union(state)
          if (modified) taskList.enqueue(destpp)
        } else {
          ann(destpp) = state
          taskList.enqueue(destpp)
        }
      }
    }
    ann
  }

  def mkString[D <: JVMEnv[_]](ann: Annotation[D]): String = {
    for ((unit, prop) <- ann) {
      unit.addTag(new StringTag(prop.toString))
    }
    Options.v().set_print_tags_in_output(true)
    val printer = Printer.v()
    val ss = new StringWriter()
    val ps = new PrintWriter(ss)
    printer.printTo(body, ps)
    ps.close()
    ss.getBuffer.toString
  }
  
  override def toString = mkString(getAnnotation)
}
