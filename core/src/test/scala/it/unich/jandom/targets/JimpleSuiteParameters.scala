/**
 * Copyright 2015 Francesca Scozzari <fscozzari@unich.it>
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

class JimpleSuiteParameters extends FunSuite with SootTests {

  val scene = initSoot("jimpletest")

  // disable all Jimple optimizations
  PhaseOptions.v().setPhaseOption("jb", "enabled:false")

  val c = scene.loadClassAndSupport("jimpletest.ParameterTest")
  val om = new SootObjectModel(scene)
  val psdom = PairSharingDomain(om)
  val numdom = BoxDoubleDomain(overReals = false)

  jimplePairSharingTests()

 
 
  def jimplePairSharingTests() {

    val jimplePairSharingTests : Seq[(String, String)] = Seq(
      "unusedParameters" ->
        """{(@this, @this), (this,this), (@this, this)}""" 
        )
 
      for ( (methodName , ps) <- jimplePairSharingTests) {
      val jmethod = new JimpleMethod(c.getMethodByName(methodName))
      val params = new Parameters[JimpleMethod] {
        val domain = new SootFrameObjectDomain(psdom)
        io = true
      }
      val inte = new TopSootInterpretation[JimpleMethod, params.type](params)
      params.interpretation = Some(inte)
      test(s"Jimple object analysis: ${methodName}") {
        try {          
          val env = Environment()
          if (!jmethod.body.getMethod.isStatic())
            env.addBinding("@this")
          for (i <- 0 until c.getMethodByName(methodName).getParameterCount())
            env.addBinding("@parameter"+i)
          for(l <- jmethod.locals )
            env.addBinding(l.getName)
//          val env = Environment(jmethod.locals map { _.getName() } :_*)
          
          val parser = new PairSharingParser(env)
          val prop = parser.parseProperty(ps).get.toSet
          val ann = jmethod.analyze(params)
          val analysisResult = ann(jmethod.lastPP.get).prop
          val oracle = psdom(prop, jmethod.localTypes(params))
          assert( analysisResult=== oracle)
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }

}
