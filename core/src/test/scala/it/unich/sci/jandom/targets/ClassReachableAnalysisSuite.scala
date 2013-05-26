/**
 * Copyright 2013 amato
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

/**
 * @author amato
 *
 */
package it.unich.sci.jandom.targets

import org.scalatest.FunSuite

import it.unich.sci.jandom.targets.jvm.ClassReachableAnalysis

import soot._

class ClassReachableAnalysisSuite extends FunSuite {
  val scene = Scene.v()
  val klassA = scene.loadClassAndSupport("javatest.A")
  val klassB = scene.loadClassAndSupport("javatest.B")
  val klassListA = scene.loadClassAndSupport("javatest.ListA")
  val klassPair = scene.loadClassAndSupport("javatest.Pair")

  val analysis = new ClassReachableAnalysis(scene)

  test("reachability") {
    expectResult(Set(klassA))(analysis.reachablesFrom(klassA))
    expectResult(Set(klassA))(analysis.reachablesFrom(klassA))
    expectResult(Set(klassA, klassListA))(analysis.reachablesFrom(klassListA))
    expectResult(Set(klassA, klassB, klassPair))(analysis.reachablesFrom(klassPair))
  }

  test("sharing") {
    expectResult(true)(analysis.mayShare(klassA, klassA))
    expectResult(false)(analysis.mayShare(klassA, klassB))
    expectResult(true)(analysis.mayShare(klassA, klassListA))
    expectResult(false)(analysis.mayShare(klassB, klassListA))
    expectResult(true)(analysis.mayShare(klassA, klassPair))
    expectResult(true)(analysis.mayShare(klassListA, klassPair))
    expectResult(true)(analysis.mayShare(klassB, klassPair))
  }
}