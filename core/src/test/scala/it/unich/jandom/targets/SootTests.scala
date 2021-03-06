/**
  * Copyright 2014, 2018 Gianluca Amato <gianluca.amato@unich.it>
  *
  * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
  * JANDOM is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * JANDOM is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.jandom.targets

import java.io.File

import soot.Scene

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * An initialization trait to mix in test suites using Soot.
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */
trait SootTests {

  /**
    * A parser for Soot identifiers, where @ is allowed as first character for
    * parameters.
    */
  trait SootIdentParser extends JavaTokenParsers {
    override def ident: Parser[String] = {
      super.ident |
        "@" ~> super.ident ^^ ("@" + _)
    }
  }

  /**
    * Initialize Soot and set classpath to the directory `dir` within the resource
    * directory.
    */
  def initSoot(dir: String = ""): Scene = {
    soot.G.reset()
    val scene = soot.Scene.v()
    scene.loadBasicClasses()
    val resourceURL = getClass.getResource("/" + dir)
    val sootTestDir = new File(resourceURL.toURI)
    scene.setSootClassPath(scene.defaultClassPath + java.io.File.pathSeparator + sootTestDir.toString)
    scene
  }
}
