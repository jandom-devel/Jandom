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
import it.unich.sci.jandom.narrowings.DefaultNarrowing
import it.unich.sci.jandom.narrowings.NoNarrowing
import it.unich.sci.jandom.ppfactories.DelayedWideningFactory
import it.unich.sci.jandom.ppfactories.MemoizingFactory
import it.unich.sci.jandom.targets.jvm.JVMEnvDomain
import it.unich.sci.jandom.targets.jvmsoot._
import it.unich.sci.jandom.widenings.DefaultWidening
import soot._
import it.unich.sci.jandom.ppfactories.DelayedNarrowingFactory
import it.unich.sci.jandom.domains.PPLProperty
import parma_polyhedra_library.C_Polyhedron
import it.unich.sci.jandom.domains.PPLDomain

/**
 * Simple test suite for the JVMSoot target.
 * @author Gianluca Amato
 *
 */
class  JVMSootSuite extends FunSuite {
  test("simple baf analysis") {
    val scene = Scene.v()
    scene.setSootClassPath(scene.defaultClassPath + ":examples/Java/")
    val c = scene.loadClass("SimpleTest",1)
    c.setApplicationClass()
    val method = new BafMethod(c.getMethodByName("longassignment"))
    val params = new Parameters(method) {
      val domain = new JVMEnvDomain(PPLCPolyhedron)      
      //wideningFactory = MemoizingFactory(method)(DelayedWideningFactory(DefaultWidening, 2))
      narrowingFactory = DelayedNarrowingFactory(NoNarrowing,2)
    }
    val ann = method.analyze(params)
    println(method.mkString(ann))
  }
 
  test("simple soot analysis") {
    val scene = Scene.v()
    scene.setSootClassPath(scene.defaultClassPath + ":examples/Java/")
    val c = scene.loadClass("SimpleTest",1)
    c.setApplicationClass()
    val method = new JimpleMethod(c.getMethodByName("longassignment"))
    val params = new Parameters(method) {
      val domain = new PPLDomain[C_Polyhedron]
      wideningFactory = MemoizingFactory(method)(wideningFactory)
      narrowingFactory = MemoizingFactory(method)(DelayedNarrowingFactory(NoNarrowing,2))
    }
    val ann = method.analyze(params)
    println(method.mkString(ann))
  }
  
}
