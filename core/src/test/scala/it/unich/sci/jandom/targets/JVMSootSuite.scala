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

import org.scalatest.FunSuite
import it.unich.sci.jandom.domains.PPLCPolyhedron
import it.unich.sci.jandom.parsers.NumericalPropertyParser
import it.unich.sci.jandom.targets.jvm._
import soot._
import it.unich.sci.jandom.domains.PPLProperty
import it.unich.sci.jandom.domains.PPLDomain

/**
 * Simple test suite for the JVMSoot target.
 * @author Gianluca Amato
 *
 */
class JVMSootSuite extends FunSuite {
  val scene = Scene.v()
  scene.setSootClassPath(scene.defaultClassPath + ":examples/Java/")
  val c = scene.loadClass("SimpleTest", 1)
  c.setApplicationClass()
  val Domain = new PPLDomain[parma_polyhedra_library.Octagonal_Shape_double]

  test("simple baf analysis") {
    val tests = Seq(//"sequential" -> "i0 == 10", "conditional" -> "i0 == 1", "loop" -> "i0 == 10",
                    "nested" -> "i0==1")
    val params = new Parameters[BafMethod] {
      val domain = new JVMEnvFixedFrameDomain(Domain)
      debugWriter = new java.io.PrintWriter(System.err)
    }

    for ((methodName, propString) <- tests) {
      val method = new BafMethod(c.getMethodByName(methodName))
      val ann = method.analyze(params)
      val env = Environment()
      val parser = new NumericalPropertyParser(env)
      val prop = parser.parseProperty(propString, Domain).get
      println(method.mkString(ann))
      params.debugWriter.flush()

      assert(ann(method.lastPP.get) === new JVMEnvFixedFrame(method.size, prop))
    }
  }
  /*
  test("simple baf analysis dynamic frame") {
    val method = new BafMethod(c.getMethodByName("nested"))
    val params = new Parameters[BafMethod] {
      val domain = new JVMEnvDynFrameDomain(PPLCPolyhedron)
      wideningFactory = MemoizingFactory(method)(DelayedWideningFactory(DefaultWidening, 2))
      narrowingFactory = MemoizingFactory(method)(DelayedNarrowingFactory(NoNarrowing, 2))
      debugWriter = new java.io.StringWriter
    }
    try {
      val ann = method.analyze(params)
      println(method.mkString(ann))
    } finally {
      println(params.debugWriter)
    }
  }

  test("simple jimple analysis") {
    val method = new JimpleMethod(c.getMethodByName("nested"))
    val params = new Parameters[JimpleMethod] {
      val domain = new PPLDomain[C_Polyhedron]
      wideningFactory =  MemoizingFactory(method)(DelayedWideningFactory(DefaultWidening, 2))
      narrowingFactory = MemoizingFactory(method)(DelayedNarrowingFactory(NoNarrowing, 2))
      debugWriter = new java.io.StringWriter
    }
    try {
      val ann = method.analyze(params)
      println(method.mkString(ann))
    } finally {
      println(params.debugWriter)
    }
  }*/
}