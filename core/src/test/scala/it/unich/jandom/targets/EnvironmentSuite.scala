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

package it.unich.jandom.targets

import org.scalatest.funsuite.AnyFunSuite

/**
 * The test suite for the Environment class
 * @author Gianluca Amato <gamato@unich.it>
 *
 */

class EnvironmentSuite  extends AnyFunSuite {
  test ("Environment standard operations") {
    val env: Environment = new Environment()
    val v1 = env.addBinding("prova")
    val v2 = env.getBindingOrAdd("micio")
    assertResult(2) { env.size }
    assertResult(Some(v1)) { env.getBinding("prova")}
    assertResult(v1) { env("prova")}
    assertResult("micio") { env(v2) }
    assertResult(Seq("prova", "micio")) { env.names }
  }
  
  test ("Environment companion object constructors") {
    val env = Environment("A","B","C")
    assertResult ("C") { env(env("C")) }
  }
  
  test("Environment equality") {
    val env1 = Environment("x","y","z")
    val env2 = Environment("x","y","z")
    assertResult (env1) { env2 }
    val env3 = Environment("a","b","c")
    assertResult (false) { env1 == env3 }
  }
}
