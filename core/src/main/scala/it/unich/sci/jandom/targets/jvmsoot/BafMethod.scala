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

import java.io.PrintWriter
import java.io.StringWriter

import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

import it.unich.sci.jandom.domains.NumericalProperty
import it.unich.sci.jandom.targets.Target
import it.unich.sci.jandom.targets.jvm.JVMEnv
import it.unich.sci.jandom.targets.jvm.JVMEnvDomain

import soot._
import soot.baf._
import soot.jimple._
import soot.options.Options
import soot.tagkit.StringTag
import soot.toolkits.graph._

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
  val chain = body.getUnits()
  //val cfg = new ExceptionalUnitGraph(body)
  //val order = new PseudoTopologicalOrderer[Unit].newList(cfg, false)

  def getAnnotation[Property]: Annotation[Property] = new Annotation[Property]

  def analyzeBlock[Property <: NumericalProperty[Property]](pp: ProgramPoint, ann: Annotation[JVMEnv[Property]]): Seq[(ProgramPoint, JVMEnv[Property])] = {
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
        case unit: IncInst =>
          unit.getConstant() match {
            case i: IntConstant  => state.iinc(unit.getLocal().getNumber(), i.value)       
          }
          
        case unit: StoreInst =>
          state.istore(unit.getLocal.getNumber)
        case unit: LoadInst =>
          state.iload(unit.getLocal.getNumber)
        case unit: IfCmpGtInst =>
          val scopy = state.clone
          scopy.if_icmpgt
          exits :+= (unit.getTarget, scopy)
          state.if_icmple
        case unit: GotoInst =>
          exits :+= (unit.getTarget, state)
        case unit: ReturnVoidInst =>
          ann(unit) = state
      }
      // We use the chain to get the fall through node. We used the first successor
      // in the CFG, but it is not clear from the documentation if we can rely on
      // this assumption.
      nextunit = chain.getSuccOf(unit)      
    } while (unit.fallsThrough() && nextunit.getBoxesPointingToThis().isEmpty())
    if (unit.fallsThrough) exits :+= (nextunit, state)
    exits
  }

  def analyze(params: Parameters): Annotation[params.Property] = {
    import scala.collection.JavaConversions._

    val ann = new Annotation[params.Property]()
    val taskList = Queue[ProgramPoint](chain.getFirst())
    ann(chain.getFirst()) = params.domain.full(body.getLocalCount())
    while (!taskList.isEmpty) {
      val pp = taskList.dequeue()
      val result = analyzeBlock(pp, ann)
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

  override def toString = mkString(getAnnotation)
}
