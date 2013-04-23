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
import it.unich.sci.jandom.domains.PPLDomain
import it.unich.sci.jandom.narrowings.NoNarrowing
import it.unich.sci.jandom.ppfactories.DelayedNarrowingFactory
import it.unich.sci.jandom.ppfactories.MemoizingFactory
import it.unich.sci.jandom.targets.jvm.JVMEnvDynFrameDomain
import it.unich.sci.jandom.targets.jvm.JVMEnvFixedFrameDomain
import it.unich.sci.jandom.targets.jvmsoot._

import parma_polyhedra_library.C_Polyhedron
import soot._

/**
 * Simple test suite for the JVMSoot target.
 * @author Gianluca Amato
 *
 */
class JVMSootSuite extends FunSuite {
  test("simple baf analysis") {
    val scene = Scene.v()
    scene.setSootClassPath(scene.defaultClassPath + ":examples/Java/")
    val c = scene.loadClass("SimpleTest", 1)
    c.setApplicationClass()
    val method = new BafMethod(c.getMethodByName("nested"))
    val params = new Parameters(method) {
      val domain = new JVMEnvFixedFrameDomain(PPLCPolyhedron)
      //wideningFactory = MemoizingFactory(method)(DelayedWideningFactory(DefaultWidening, 2))
      narrowingFactory = MemoizingFactory(method)(DelayedNarrowingFactory(NoNarrowing, 2))
    }
    val ann = method.analyze(params)
    println(method.mkString(ann))
  }

  test("simple baf analysis dynamic frame") {
    val scene = Scene.v()
    scene.setSootClassPath(scene.defaultClassPath + ":examples/Java/")
    val c = scene.loadClass("SimpleTest", 1)
    c.setApplicationClass()
    val method = new BafMethod(c.getMethodByName("nested"))
    val params = new Parameters(method) {
      val domain = new JVMEnvDynFrameDomain(PPLCPolyhedron)
      //wideningFactory = MemoizingFactory(method)(DelayedWideningFactory(DefaultWidening, 2))
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

  test("simple soot analysis") {
    val scene = Scene.v()
    scene.setSootClassPath(scene.defaultClassPath + ":examples/Java/")
    val c = scene.loadClass("SimpleTest", 1)
    c.setApplicationClass()
    val method = new JimpleMethod(c.getMethodByName("nested"))
    val params = new Parameters(method) {
      val domain = new PPLDomain[C_Polyhedron]
      wideningFactory = MemoizingFactory(method)(wideningFactory)
      narrowingFactory = MemoizingFactory(method)(DelayedNarrowingFactory(NoNarrowing, 2))
    }
    val ann = method.analyze(params)
    println(method.mkString(ann))
  }

}
