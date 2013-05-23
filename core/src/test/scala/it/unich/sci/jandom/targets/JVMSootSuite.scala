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
import scala.collection.mutable.ArrayStack
import org.scalatest.FunSuite
import it.unich.sci.jandom.domains.numerical.PPLCPolyhedron
import it.unich.sci.jandom.domains.objects.ObjectNumericalDomain
import it.unich.sci.jandom.domains.objects.ObjectNumericalProperty
import it.unich.sci.jandom.parsers.NumericalPropertyParser
import it.unich.sci.jandom.targets.jvm._
import soot._
import it.unich.sci.jandom.domains.objects.PairSharingDomain

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

  test("Baf analysis with fixed frame environment") {
    val tests = Seq(
      ("sequential", Array(0), "i0 == 10"),
      ("conditional", Array(0), "i0 == 1"),
      ("loop", Array(0), "i0 == 10"),
      ("nested", Array(0, 1, -1), "i0 >= 9 && i1 == 10"),
      // "longassignment" -> "true",  unsupported bytecode
      ("topologicalorder", Array(0), "i0 >= 3 && i0 <= 4"))

    val params = new Parameters[BafMethod] {
      val domain = new JVMEnvFixedFrameDomain(JVMSootSuite.this.numdomain)
      //wideningFactory = MemoizingFactory(method)(DelayedWideningFactory(DefaultWidening, 2))
      //narrowingFactory = MemoizingFactory(method)(DelayedNarrowingFactory(NoNarrowing, 2))
      //debugWriter = new java.io.StringWriter
    }
    for ((methodName, frame, propString) <- tests) {
      val method = new BafMethod(c.getMethodByName(methodName))
      val ann = method.analyze(params)
      val env = Environment()
      val parser = new NumericalPropertyParser(env)
      val prop = parser.parseProperty(propString, numdomain).get
      val jvmenv = new JVMEnvDynFrame(frame, ArrayStack(), prop).toJVMEnvFixedFrame
      assert(ann(method.lastPP.get) === jvmenv, s"In the analysis of ${methodName}")
    }
  }

  test("Jimple numerical analysis") {
    val tests = Seq(
      "sequential" -> "v0 == 0 && v1 == 10 && v2 == 10",
      "conditional" -> "v0 == 0 && v1 == 0 && v2 == 1 && v3==v3",
      "loop" -> "v0 >= 10 && v0 <= 11",
      "nested" -> "v0 >= v1 - 1 && v1 >= 10 && v1 <= 11 && v2==v2",
      "longassignment" -> "v0 >= 0 && v1 <= 11 && v1 >= 10 && v2 == v2 && v3 == v3 && v4 == v4",
      "topologicalorder" -> "v0 == 1 && v1 - v2 == -1 &&  v2 >= 3 && v2 <= 4",
      "objcreation" -> "v0 == v0 && v1 == v1 && v2 == v2 && v3 == v3")

    val params = new Parameters[JimpleMethod] {
      val domain = new ObjectNumericalDomain(JVMSootSuite.this.numdomain)
      //debugWriter = new java.io.PrintWriter(System.err)
    }
    for ((methodName, propString) <- tests) {
      val method = new JimpleMethod(c.getMethodByName(methodName))
      try {
        val ann = method.analyze(params)
        val env = Environment()
        val parser = new NumericalPropertyParser(env)
        val prop = parser.parseProperty(propString, numdomain).get
        val objprop = new ObjectNumericalProperty(prop, BitSet(0 until prop.dimension: _*))
        assert(ann(method.lastPP.get) === objprop, s"In the analysis of ${methodName}")
      } finally {
        //params.debugWriter.flush()
      }
    }
  }

  test("Jimple object analysis") {
    val tests = Seq(
      "sequential" -> "v0 == 0 && v1 == 10 && v2 == 10",
      "conditional" -> "v0 == 0 && v1 == 0 && v2 == 1 && v3==v3",
      "loop" -> "v0 >= 10 && v0 <= 11",
      "nested" -> "v0 >= v1 - 1 && v1 >= 10 && v1 <= 11 && v2==v2",
      "longassignment" -> "v0 >= 0 && v1 <= 11 && v1 >= 10 && v2 == v2 && v3 == v3 && v4 == v4",
      "topologicalorder" -> "v0 == 1 && v1 - v2 == -1 &&  v2 >= 3 && v2 <= 4",
      "objcreation" -> "v0 == v0 && v1 == v1 && v2 == v2 && v3 == v3"
      )

    val params = new Parameters[JimpleMethod] {
      val domain = new PairSharingDomain()
      //debugWriter = new java.io.PrintWriter(System.err)
    }
    for ((methodName, propString) <- tests) {
      val method = new JimpleMethod(c.getMethodByName(methodName))
      try {
        val ann = method.analyze(params)
        println (method.mkString(ann))
      } finally {
        //params.debugWriter.flush()
      }
    }
  }

}
