/**
  * Copyright 2013, 2015, 2018 Gianluca Amato, Francesca Scozzari
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

package it.unich.jandom.targets

import it.unich.jandom.domains.numerical.ppl.PPLDomain
import it.unich.jandom.domains.objects.{PairSharingDomain, UP}
import it.unich.jandom.parsers.NumericalPropertyParser
import it.unich.jandom.targets.jvmsoot._
import org.scalatest.funsuite.AnyFunSuite
import parma_polyhedra_library.C_Polyhedron

import scala.collection.JavaConverters._

/**
  * Simple test suite for the JVMSoot target.
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  * @author Francesca Scozzari <fscozzari@unich.it>
  *
  */
class JVMSootSuite extends AnyFunSuite with SootTests {

  private val scene = initSoot()
  private val c = scene.loadClassAndSupport("javatest.SimpleTest")
  private val om = new SootObjectModel(scene)
  private val psdom = new PairSharingDomain(om)

  private val numdomain = PPLDomain[C_Polyhedron]()

  bafTests()
  jimpleInterProceduralNumTests()
  jimpleInterProceduralPSTests()

  def bafTests() {
    val bafTests = Seq(
      "sequential" -> "i2 == 10 && i0==i0 && i1==i1",
      "conditional" -> "z0 == 1 && z1==z1 && z2==z2 && z3==z3",
      "loop" -> "i0 >= 10 && i0 <= 11",
      "nested" -> "i0 >= i1 - 1 && i1 >= 10 && i1 <= 11 && i2==i2",
      "longassignment" -> "i0 >= 0 && i1 >= 10 && i2==i2 && i3==i3 && i4==i4",
      "topologicalorder" -> "b0 >= 3 && b0<=4 && b1 == b1 && b2==b2")

    val params: Parameters[BafMethod] {val domain: SootFrameNumericalDomain} = new Parameters[BafMethod] {
      val domain = new SootFrameNumericalDomain(numdomain)
      //debugWriter = new java.io.PrintWriter(System.err)
    }

    for ((methodName, propString) <- bafTests) {
      val method = new BafMethod(c.getMethodByName(methodName), params.io)

      test(s"Baf numerical analysis: $methodName") {
        try {
          val ann = method.analyze(params)
          val env = Environment()
          val parser = new NumericalPropertyParser(env)
          val prop = parser.parseProperty(propString, params.domain.numdom).get
          assert(ann(method.lastPP.get).prop === prop)
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }

  def jimpleInterProceduralPSTests() {
    val jimplePairSharingTests: Seq[(String, Set[UP[Int]])] = Seq(
      "sequential" -> Set(),
      "objcreation" -> Set(),
      "class_parametric" -> Set(UP(0, 0), UP(0, 1), UP(1, 1)),
      "pair_one" -> Set(UP(0, 0), UP(0, 2), UP(1, 1), UP(2, 2)))

    for ((methodName, prop) <- jimplePairSharingTests) {
      val method = c.getMethodByName(methodName)
      val params: Parameters[JimpleMethod] {val domain: SootFrameObjectDomain} = new Parameters[JimpleMethod] {
        val domain = new SootFrameObjectDomain(psdom)
        io = true
      }
      val jmethod = new JimpleMethod(method, params.io)
      val inte = new JimpleRecursiveInterpretation[params.type](scene, params)
      params.interpretation = Some(inte)
      test(s"Jimple inter-procedural sharing analysis: $methodName") {
        val input = params.domain.top(c.getMethodByName(methodName).getParameterTypes.asScala)
        try {
          inte.compute(method, input)
          assert(inte(method, input).prop === psdom(prop, jmethod.outputTypes))
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }

  def jimpleInterProceduralNumTests() {
    val jimpleNumericalTests = Seq(
      "sequential" -> "0 == 0",
      "parametric_dynamic" -> "this==this && i0 == i0 && i1 == i1 && i2 == i2",
      "parametric_static" -> "i0 == i0 && i1 == i1 && i2 == i0 +i1",
      "parametric_caller" -> "i0 == i0 && i1== i1 && i2==7",
      "recursa" -> "-i0 + i1 >= 0 && i1 >= 0")

    for ((methodName, propString) <- jimpleNumericalTests) {
      val method = c.getMethodByName(methodName)
      val params: Parameters[JimpleMethod] {val domain: SootFrameNumericalDomain} = new Parameters[JimpleMethod] {
        val domain = new SootFrameNumericalDomain(JVMSootSuite.this.numdomain)
        io = true
      }
      val inte = new JimpleRecursiveInterpretation[params.type](scene, params)
      params.interpretation = Some(inte)
      test(s"Jimple inter-procedural numerical analysis: $methodName") {
        val env = Environment()
        val parser = new NumericalPropertyParser(env)
        val input = params.domain.top(SootCFG.inputTypes(c.getMethodByName(methodName)))
        try {
          inte.compute(method, input)
          val prop = parser.parseProperty(propString, params.domain.numdom).get
          assert(inte(method, input).prop === prop)
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }
}
