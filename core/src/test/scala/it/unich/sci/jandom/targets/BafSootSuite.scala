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
import soot._
import it.unich.sci.jandom.targets.jvmsoot.BafMethod
import java.io.StringWriter
import java.io.PrintWriter
import it.unich.sci.jandom.targets.jvm.JVMEnvDomain
import it.unich.sci.jandom.domains.PPLCPolyhedron
import soot.options.Options

/**
 * Simple test suite for the Baf analyzer
 * @author Gianluca Amato
 *
 */
class BafSootSuite extends FunSuite {
  test("simple method analysis") {
    val scene = Scene.v()
    scene.setSootClassPath(scene.defaultClassPath + ":examples/Java/")
    val c = scene.loadClass("SimpleTest",1)
    c.setApplicationClass()
    val method = new BafMethod(c.getMethodByName("conditional"))
    val params = new Parameters(method) {
      val domain = new JVMEnvDomain(PPLCPolyhedron)
    }
    println(method)
    val ann = method.analyze(params)
    println(method.mkString(ann))
  }
}
