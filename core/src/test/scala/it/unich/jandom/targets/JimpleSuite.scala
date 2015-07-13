/**
 * Copyright 2015 Francesca Scozzari <fscozzari@unich.it>, Gianluca Amato <gamato@unich.it>
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

import org.scalatest.FunSuite
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.objects.PairSharingDomain
import it.unich.jandom.parsers.NumericalPropertyParser
import it.unich.jandom.targets.jvmsoot._
import soot._
import it.unich.jandom.parsers.PairSharingParser

/**
 * Simple test suite for the Soot Jimple target. Differently from JVMSootSuite, we
 * directly analyze Jimple code.
 * @author Francesca Scozzari <fscozzari@unich.it>
 * @author Gianluca Amato <gamato@unich.it>
 */

class JimpleSuite extends FunSuite with SootTests {

  val scene = initSoot("jimpletest")

  // disable all Jimple optimizations
  PhaseOptions.v().setPhaseOption("jb", "enabled:false")

  val c = scene.loadClassAndSupport("jimpletest.SimpleTest")
  val om = new SootObjectModel(scene)
  val psdom = PairSharingDomain(om)
  val numdom = BoxDoubleDomain(overReals = false)

  jimpleNumTests()
  jimplePairSharingTests()

  def jimpleNumTests() {
    val jimpleNumericalTests = Seq(
      "sequential" ->
        Seq(None -> false -> "b0 == 0 && b1 == 10 && i2 == 10"),
      "conditional" ->
        Seq(None -> false -> "b0 == 0 && z0 == 0 && z1 == 1"),
      "loop" ->
        Seq(None -> false -> "i0 >= 10 && i0 <= 11"),
      "nested" ->
        Seq(None -> false -> "i0 >= 0 && i1 >= 10 && i1 <= 11"),
      "longassignment" ->
        Seq(None -> false -> "i0 >= 0 && i1 >= 10 && i1 <= 11"),
      "topologicalorder" ->
        Seq(None -> false -> "z0 == 1 && b0 == 2 && i1 == 3"),
      "parametric_static" -> Seq(
        None -> true -> "i0 == i0", // cannot parse empty strings
        None -> true -> "@parameter0 == i0 && @parameter1 == i1", // pi's are parameters
        Some("@parameter0 == 0") -> true -> "@parameter0 == 0 && i0 == 0",
        Some("@parameter0 == 0") -> true -> "i0 == 0 && @parameter0 == i0 && @parameter1 == i1"),
      "parametric_dynamic" ->
        Seq(None -> true -> "i0 == i0"),
      "parametric_caller" ->
        Seq(None -> true -> "b3 ==3 && b4 ==4 && i2 ==7 && @parameter0 == i0 && @parameter1 == i1"))

    for ((methodName, instances) <- jimpleNumericalTests; (((input, ifIo), propString), i) <- instances.zipWithIndex) {
      val method = new JimpleMethod(c.getMethodByName(methodName))
      val params = new Parameters[JimpleMethod] {
        val domain = new SootFrameNumericalDomain(numdom)
        io = true
        interpretation = Some(new JimpleInterpretation(this))
      }
      test(s"Jimple numerical analysis: ${methodName} ${if (i > 0) i + 1 else ""}") {
        val parser = new NumericalPropertyParser(method.environment) with SootIdentParser
        val inputParser = new NumericalPropertyParser(method.inputEnvironment) with SootIdentParser
        try {
          val ann = input match {
            case None =>
              method.analyze(params)
            case Some(input) =>
              val prop = inputParser.parseProperty(input, params.domain.numdom).get
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

    val jimplePairSharingTests: Seq[(String, String)] = Seq(
      "sequential" -> "{}",
      "conditional" -> "{}",
      "loop" -> "{}",
      "nested" -> "{}",
      "longassignment" -> "{}",
      "topologicalorder" -> "{}",
      "complexif" -> "{}",
      "objcreation" -> """{(r2, r0), (r2, $r6), (r2, $r5), ($r6, r0), ($r6, r8), ($r6 , $r7), (r8, r8), ($r3, r8), (r2, r2), ($r5, $r5),
        ($r5, $r3), (r0, $r3), ( $r4, $r4), ($r7, $r7), ($r5, r8), ($r5, $r6), ($r6, $r3), (r0, r8), (r1, r1), (r2, $r3), ($r5, $r7), ($r3, $r3),(r2, $r7), ($r6, $r6),
        (r2, r8), ($r5, r0), ($r7, r0), (r0, r0), (r1, $r4), ($r7, r8), ($r7, $r3)}""",
      "classrefinement" ->
        """{(r0, r0), (r0, $r3), (r1, r1), (r1, r2), (r1, $r4), (r1, $r5), (r2, r2), (r2, $r4), (r2, $r5), (r2, r6), ($r3, $r3), ($r4, $r4), ($r4, $r5),
        ($r5, $r5), ($r5, r6), (r6, r6)}""",
      "class_parametric" ->
        """{(@parameter0, @parameter0), (r0, r0), (r1, r1), ($r2, $r2),($r3, $r3),  (r4, r4),
        (@parameter0, r0), (@parameter0, r1), (@parameter0, $r2),
        (r0, r1), (r0, $r2),
        (r1, $r2),
        ($r3, r4)}""")

    for ((methodName, ps) <- jimplePairSharingTests) {
      val jmethod = new JimpleMethod(c.getMethodByName(methodName))
      val params = new Parameters[JimpleMethod] {
        val domain = new SootFrameObjectDomain(psdom)
        io = true
      }
      val inte = new TopSootInterpretation[JimpleMethod, params.type](params)
      params.interpretation = Some(inte)
      test(s"Jimple object analysis: ${methodName}") {
        try {
          val parser = new PairSharingParser(jmethod.environment) with SootIdentParser
          val prop = parser.parseProperty(ps).get
          val ann = jmethod.analyze(params)
          assert(ann(jmethod.lastPP.get).prop === psdom(prop, jmethod.localTypes(params)))
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }
}
