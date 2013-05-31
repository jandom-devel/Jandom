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

package it.unich.sci.jandom.targets

import scala.collection.immutable.BitSet
import scala.collection.immutable.Stack
import org.scalatest.FunSuite
import it.unich.sci.jandom.domains.numerical.PPLCPolyhedron
import it.unich.sci.jandom.domains.objects.UP
import it.unich.sci.jandom.parsers.NumericalPropertyParser
import it.unich.sci.jandom.targets.jvmsoot._
import soot._
import it.unich.sci.jandom.domains.objects.PairSharingDomain
import soot.baf.WordType

/**
 * Simple test suite for the JVMSoot target.
 * @author Gianluca Amato
 *
 */
class JVMSootSuite extends FunSuite {
  val scene = Scene.v()
  val c = scene.loadClassAndSupport("javatest.SimpleTest")
  c.setApplicationClass()
  val numdomain = PPLCPolyhedron

  bafTests()
  jimpleNumTests()
  jimplePairSharingTests()

  def bafTests() {
    val bafTests = Seq(
      "sequential" -> "i2 == 10",
      "conditional" -> "z0 == 1",
      "loop" -> "i0 >= 10 && i0 <= 11",
      "nested" -> "i0 >= i1 - 1 && i1 >= 10 && i1 <= 11 && i2==i2",
      "longassignment" -> "i0 >= 0 && i1 >= 10 && i2==i2",
      "topologicalorder" -> "b0 >= 3 && b0<=4")

    val params = new Parameters[BafMethod] {
      val domain = new SootFrameNumericalDomain(JVMSootSuite.this.numdomain)
      //debugWriter = new java.io.PrintWriter(System.err)
    }
    for ((methodName, propString) <- bafTests) {
      val method = new BafMethod(c.getMethodByName(methodName))

      test(s"Baf numerical analysis: ${methodName}") {
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

  def jimpleNumTests() {
    val jimpleNumericalTests = Seq(
      "sequential" ->
        Seq(None -> false -> "v0 == 0 && v1 == 10 && v2 == 10"),
      "conditional" ->
        Seq(None -> false -> "v0 == 0 && v1 == 0 && v2 == 1 && v3==v3"),
      "loop" ->
        Seq(None -> false -> "v0 >= 10 && v0 <= 11"),
      "nested" ->
        Seq(None -> false -> "v0 >= v1 - 1 && v1 >= 10 && v1 <= 11 && v2==v2"),
      "longassignment" ->
        Seq(None -> false -> "11*v0 - 33*v1 >= -63 && v1 >=10 && v2 == v2 && v3 == v3 && v4 == v4"),
      "topologicalorder" ->
        Seq(None -> false -> "v0 == 1 && v1 - v2 == -1 &&  v2 >= 3 && v2 <= 4"),
      "parametric_static" -> Seq(
        None -> false -> "i0 + i1 - i2 == 0",
        None -> true -> "i0 + i1 - i2 == 0  && p0 == i0 && p1 == i1",
        Some("i0 == 0 && i1==i1 && i2==i2") -> false -> "i0==0 && i1 - i2 == 0",
        Some("i0 == 0 && i1==i1 && i2==i2") -> true -> "i0==0 && i1 - i2 == 0 && p0 == i0 && p1 == i1"),
      "parametric_dynamic" ->
        Seq(None -> false -> "r0==r0 && i0 + i1 - i2 == 0 && i3==i3"))

    for ((methodName, instances) <- jimpleNumericalTests; (((input, ifIo), propString), i) <- instances.zipWithIndex) {
      val method = new JimpleMethod(c.getMethodByName(methodName))
      val params = new Parameters[JimpleMethod] {
        io = ifIo
        val domain = new SootFrameNumericalDomain(JVMSootSuite.this.numdomain)
        //debugWriter = new java.io.PrintWriter(System.err)
      }
      test(s"Jimple numerical analysis: ${methodName} ${if (i > 0) i + 1 else ""}") {
        val env = Environment()
        val parser = new NumericalPropertyParser(env)
        try {
          val ann = input match {
            case None => method.analyze(params)
            case Some(input) =>
              val prop = parser.parseProperty(input, params.domain.numdom).get
              method.analyzeFromInput(params)(params.domain(prop, IntType.v()))
          }
          val prop = parser.parseProperty(propString, params.domain.numdom).get
          assert(ann(method.lastPP.get).prop === prop)
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }

  def jimplePairSharingTests() {
    val jimplePairSharingTests: Seq[(String, Set[UP[Int]])] = Seq(
      "sequential" -> Set(),
      "conditional" -> Set(),
      "loop" -> Set(),
      "nested" -> Set(),
      "longassignment" -> Set(),
      "topologicalorder" -> Set(),
      "complexif" -> Set(),
      "objcreation" -> Set(UP(0, 0), UP(0, 5), UP(1, 1), UP(1, 5), UP(5, 6), UP(6, 7), UP(2, 2), UP(0, 1), UP(0, 6), UP(6, 6), UP(5, 7), UP(3, 6), UP(3, 5),
        UP(1, 3), UP(1, 8), UP(2, 4), UP(0, 7), UP(5, 8), UP(3, 7), UP(0, 3), UP(1, 7), UP(7, 8), UP(8, 8), UP(0, 8), UP(4, 4), UP(5, 5), UP(7, 7), UP(3, 3),
        UP(3, 8), UP(1, 6), UP(6, 8)),
      "classrefinement" -> Set(UP(0, 0), UP(1, 1), UP(5, 6), UP(2, 2), UP(0, 1), UP(2, 3), UP(6, 6), UP(4, 5), UP(3, 6), UP(3, 5), UP(2, 4), UP(3, 4),
        UP(2, 5), UP(4, 4), UP(5, 5), UP(3, 3)),
      "class_parametric" -> Set(UP(0, 0), UP(0, 5), UP(1, 1), UP(1, 5), UP(2, 2), UP(0, 1), UP(0, 2), UP(3, 4), UP(1, 2), UP(2, 5),UP(4, 4),UP(5, 5),UP(3, 3)))

    for ((methodName, ps) <- jimplePairSharingTests) {
      val method = new JimpleMethod(c.getMethodByName(methodName))
      val classAnalysis = new ClassReachableAnalysis(scene)
      val params = new Parameters[JimpleMethod] {
        val domain = new SootFramePairSharingDomain(classAnalysis)
        io = true
        //debugWriter = new java.io.PrintWriter(System.err)
      }
      test(s"Jimple object analysis: ${methodName}") {
        try {
          val ann = method.analyze(params)
          //println(method.mkString(params)(ann))
          assert(ann(method.lastPP.get).prop === params.domain.dom.Property(ps, method.locals.size + method.body.getMethod().getParameterCount()))
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }
}
