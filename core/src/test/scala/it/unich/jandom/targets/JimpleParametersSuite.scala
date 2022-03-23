/**
  * Copyright 2015 Francesca Scozzari <fscozzari@unich.it>
  * Copyright 2018 Gianluca Amato <gianluca.amato@unich.it>
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

import org.scalatest.funsuite.AnyFunSuite
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.objects.PairSharingDomain
import it.unich.jandom.targets.jvmsoot._
import soot._
import it.unich.jandom.parsers.PairSharingParser

/**
  * Simple test suite for the Soot Jimple target. Differently from JVMSootSuite, we
  * directly analyze Jimple code. Differently from JimpleSuite, we stress parameter
  * passing.
  *
  * @author Francesca Scozzari <fscozzari@unich.it>
  * @author Gianluca Amato <gamato@unich.it>
  */

class JimpleParametersSuite extends AnyFunSuite with SootTests {

  private val scene = initSoot("jimpletest")

  // disable all Jimple optimizations
  PhaseOptions.v().setPhaseOption("jb", "enabled:false")

  private val c = scene.loadClassAndSupport("jimpletest.ParameterTest")
  val om = new SootObjectModel(scene)
  val psdom = PairSharingDomain(om)
  val numdom = BoxDoubleDomain()

  jimplePairSharingTests()
  jimplePairSharingTestsFromInput()

  def jimplePairSharingTests(): scala.Unit = {

    val jimplePairSharingTests: Seq[(String, String)] = Seq(
      "emptyInt" ->
        """{(@this, @this), (this,this), (@this, this)}""",
      "emptyNull" ->
        """{(@this, @this), (this,this), (@this, this)}""",
      "emptyThis" ->
        """{(@this, @this), (this,this),
            (@this, this)}""",
      "unusedParameters" ->
        """{(@this, @this), (this,this), (@this, this),
           (@parameter1,@parameter1), (b,b), (@parameter1,b),
            (@this, @parameter1), (@this, b), (this, @parameter1), (this, b) }""",
      "returnParameter" ->
        """{(@this, @this), (this,this), (@this, this),
            (@parameter1,@parameter1), (b,b), (@parameter1,b),
            (@this, @parameter1), (@this, b), (this, @parameter1), (this, b)}""",
      "returnThis" ->
        """{(@this, @this), (this,this), (@this, this),
            (@parameter1,@parameter1), (b,b), (@parameter1,b),
            (@this, @parameter1), (@this, b), (this, @parameter1), (this, b)}""",
      "create" ->
        """{(@this, @this), (this,this), (@this, this),
            (a,a), (temp$0,temp$0), (a,temp$0)}""",
      "create2" ->
        """{(@this, @this), (this,this), (@this, this),
            (@parameter1,@parameter1),   (@parameter1,  @this), (@parameter1,  this),
            (a,a), (temp$0,temp$0), (a,temp$0),
            (y,y), (y,a), (y,temp$0)}""",
      "createCall" ->
        """{(@this, @this), (this,this), (@this, this),
            (@parameter1,@parameter1),   (@parameter1,  @this), (@parameter1,  this),
            (a,a), (temp$0,temp$0), (a,temp$0),
            (y,y), (y,a), (y,temp$0),
            (temp$1,temp$1), (temp$1,a), (temp$1,y), (temp$1,temp$0)}""",
      "emptyIntS" ->
        """{}""",
      "emptyNullS" ->
        """{}""",
      "unusedParametersS" ->
        """{(@parameter1,@parameter1), (b,b), (@parameter1,b)}""",
      "returnParameterS" ->
        """{(@parameter1,@parameter1), (b,b), (@parameter1,b)}""")

    for ((methodName, ps) <- jimplePairSharingTests) {
      val params: Parameters[JimpleMethod] {val domain: SootFrameObjectDomain} = new Parameters[JimpleMethod] {
        val domain = new SootFrameObjectDomain(psdom)
        io = true
      }
      val inte = new TopSootInterpretation[JimpleMethod, params.type](params)
      val jmethod = new JimpleMethod(c.getMethodByName(methodName), params.io)
      params.interpretation = Some(inte)
      test(s"Jimple object analysis: $methodName") {
        try {
          val env = jmethod.environment
          val parser = new PairSharingParser(env) with SootIdentParser
          val prop = parser.parseProperty(ps).get
          val oracle = psdom(prop, jmethod.localTypes)
          val ann = jmethod.analyze(params)
          val analysisResult = ann(jmethod.lastPP.get).prop
          assert(analysisResult === oracle)
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }

  def jimplePairSharingTestsFromInput(): scala.Unit = {

    // analyze from an input property where @this is not null and all the other variables are null

    val jimplePairSharingTests: Seq[(String, String)] = Seq(
      "emptyInt" ->
        """{(@this, @this), (this,this), (@this, this)}""",
      "emptyNull" ->
        """{(@this, @this), (this,this), (@this, this)}""",
      "emptyThis" ->
        """{(@this, @this), (this,this),
            (@this, this)}""",
      "unusedParameters" ->
        """{(@this, @this), (this,this), (@this, this) }""",
      "returnParameter" ->
        """{(@this, @this), (this,this), (@this, this) }""",
      "returnThis" ->
        """{(@this, @this), (this,this), (@this, this)}""",
      "create" ->
        """{(@this, @this), (this,this), (@this, this),
            (a,a), (temp$0,temp$0), (a,temp$0)}""",
      "create2" ->
        """{(@this, @this), (this,this), (@this, this),
            (a,a), (temp$0,temp$0), (a,temp$0),
            (y,y), (y,a), (y,temp$0)}""",
      "createCall" ->
        """{(@this, @this), (this,this), (@this, this),
           (a,a), (temp$0,temp$0), (a,temp$0),
            (y,y), (y,a), (y,temp$0),
            (temp$1,temp$1), (temp$1,a), (temp$1,y), (temp$1,temp$0)}""",
      "emptyIntS" ->
        """{}""",
      "emptyNullS" ->
        """{}""",
      "unusedParametersS" ->
        """{}""",
      "returnParameterS" ->
        """{}""")

    for ((methodName, ps) <- jimplePairSharingTests) {
      val params: Parameters[JimpleMethod] {val domain: SootFrameObjectDomain} = new Parameters[JimpleMethod] {
        val domain = new SootFrameObjectDomain(psdom)
        io = true
      }
      val jmethod = new JimpleMethod(c.getMethodByName(methodName), params.io)
      val inte = new TopSootInterpretation[JimpleMethod, params.type](params)
      params.interpretation = Some(inte)
      test(s"Jimple object analysis from input: $methodName") {
        try {
          val env = jmethod.environment
          val parser = new PairSharingParser(env) with SootIdentParser
          val prop = parser.parseProperty(ps).get
          val oracle = psdom(prop, jmethod.localTypes)
          val inputProp = if (jmethod.method.isStatic)
            psdom.bottom(jmethod.inputTypes)
          else
            psdom(parser.parseProperty("{(@this, @this)}").get, jmethod.inputTypes)
          val inputProp2 = params.domain.Property(inputProp.asInstanceOf[params.domain.dom.Property], jmethod.inputTypes.toList.reverse, Map.empty)
          val ann = jmethod.analyzeFromInput(params)(inputProp2)
          val analysisResult = ann(jmethod.lastPP.get).prop
          assert(analysisResult === oracle)
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }
}
