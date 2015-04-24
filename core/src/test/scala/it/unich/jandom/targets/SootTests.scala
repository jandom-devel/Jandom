/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
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

/**
 * A initialization trait to mix in test suites using Soot.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait SootTests {

  /**
   * Initialize Soot and set classpath to the directory `dir` within the resource
   * directory.
   */
  def initSoot(dir: String = "") = {
    soot.G.reset()
    val scene = soot.Scene.v()
    scene.loadBasicClasses()
    val resourceURL = getClass().getResource("/"+dir)
    val sootTestDir = new File(resourceURL.toURI())
    scene.setSootClassPath(sootTestDir.toString())
    scene
  }

}
