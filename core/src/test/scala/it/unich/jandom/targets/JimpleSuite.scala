/**
 * Copyright 2015 Francesca Scozzari
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

import org.scalatest.FunSuite

import it.unich.jandom.targets.jvmsoot._
import soot._
import soot.jimple.Jimple;
import soot.jimple.StringConstant;
import soot.util.Chain;
import java.util.ArrayList;
import it.unich.jandom.domains.objects.UP
import it.unich.jandom.domains.objects.PairSharingDomain

/**
 * Simple test suite for the Soot Jimple target.
 * @author Francesca Scozzari
 *
 */

class JimpleSuite extends FunSuite with SootTests {

  // disable all jimple optimizations 
  PhaseOptions.v().setPhaseOption("jb", "enabled:false")
  
  // read only jimple files
  val jimpleTestDir = java.nio.file.Paths.get(System.getProperty("user.dir"),"src","test","resources","jimple")
  scene.setSootClassPath(jimpleTestDir.toString)
  
  val c = scene.loadClassAndSupport("javatest.SimpleTest")
  val om = new SootObjectModel(scene)
  val psdom = new PairSharingDomain(om)
  
  jimplePairSharingTests()

  def jimplePairSharingTests() {
    
    val jimplePairSharingTests : Seq[(String, (String, Seq[String], Seq[soot.Type]))] = Seq(
      "sequential" -> ("{}", Seq(), Seq()),
      "conditional" -> ("{}", Seq(), Seq()),
      "loop" -> ("{}", Seq(), Seq()),
      "nested" -> ("{}", Seq(), Seq()),
      "longassignment" -> ("{}", Seq(), Seq()),
      "topologicalorder" -> ("{}", Seq(), Seq()),
      "complexif" -> ("{}", Seq(), Seq()),
      "objcreation" -> 
        ("{(r2, r0), (r2, $r6), (r2, $r5), ($r6, r0), ($r6, r8), ($r6 , $r7), (r8, r8), ($r3, r8), (r2, r2), ($r5, $r5), ($r5, $r3), (r0, $r3), ( $r4, $r4), ($r7, $r7), ($r5, r8), ($r5, $r6), ($r6, $r3), (r0, r8), (r1, r1), (r2, $r3), ($r5, $r7), ($r3, $r3),(r2, $r7), ($r6, $r6), (r2, r8), ($r5, r0), ($r7, r0), (r0, r0), (r1, $r4), ($r7, r8), ($r7, $r3)}",
        Seq("r2", "$r5", "$r6", "$r7", "r0", "r1", "$r3", "$r4", "r8"), 
        Seq()), // variable types - NOT USED
            // javatest.ListA, javatest.ListA, javatest.ListA, javatest.ListA, javatest.A, javatest.A, javatest.A, javatest.A, javatest.A)
     "classrefinement" -> 
        ("{(r0, r0), (r0, $r3), (r1, r1), (r1, r2), (r1, $r4), (r1, $r5), (r2, r2), (r2, $r4), (r2, $r5), (r2, r6), ($r3, $r3), ($r4, $r4), ($r4, $r5), ($r5, $r5), ($r5, r6), (r6, r6)}",
        Seq("r2", "$r5", "r1", "$r4", "r0", "$r3", "r6"),
        Seq()),
      "class_parametric" -> 
        ("{(r0, r0), (r0, r1), (r0, $r2), (r0, @p0), (r1, r1), (r1, $r2), (r1, @p0), ($r2, $r2), ($r2, @p0), ($r3, $r3),  ($r3, r4), (r4, r4), (@p0, @p0)}",
        Seq("r0", "$r3", "r4", "r1", "$r2", "@p0"),
        Seq())
       )
     
      for ( (methodName , (ps, varNames, varTypes)) <- jimplePairSharingTests) {
      val jmethod = new JimpleMethod(c.getMethodByName(methodName))
      val params = new Parameters[JimpleMethod] {
        val domain = new SootFrameObjectDomain(psdom)
        io = true
        //debugWriter = new java.io.PrintWriter(System.err)
      }
      val inte = new TopSootInterpretation[JimpleMethod, params.type](params)
      params.interpretation = Some(inte)
      test(s"Jimple object analysis: ${methodName}") {
        try {
          val ann = jmethod.analyze(params)
          print(jmethod.mkString(params)(ann))
          assert(ann(jmethod.lastPP.get).prop === psdom.parseProperty(ps, varNames ,jmethod.localTypes(params)))
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }

}

