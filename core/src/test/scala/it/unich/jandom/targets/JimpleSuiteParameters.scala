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
  jimplePairSharingTestsFromInput()
 
 
  def jimplePairSharingTests() {

    val jimplePairSharingTests : Seq[(String, String)] = Seq(
    "emptyInt" ->
        """{(@this, @this), (this,this), (@this, this)}""",
    "emptyNull" -> 
        """{(@this, @this), (this,this), (@this, this)}""",
    "emptyThis" -> 
//        """{(@this, @this), (this,this), (@return,@return), 
//            (@this, this), (@this,@return), (this,@return)}"""
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
         if (jmethod.body.getMethod.getReturnType() != VoidType.v())
            env.addBinding("@return")
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
  
  def jimplePairSharingTestsFromInput() {
    // analyze from an input property where @this is not null and all the other variables are null
    val jimplePairSharingTests : Seq[(String, String)] = Seq(
   "emptyIntS" ->
        """{}""",
    "emptyNullS" -> 
        """{}""",
    "unusedParametersS" ->
        """{}""",  
    "returnParameterS" ->
        """{}""")
    
      for ( (methodName , ps) <- jimplePairSharingTests) {
      val jmethod = new JimpleMethod(c.getMethodByName(methodName))
      val params = new Parameters[JimpleMethod] {
        val domain = new SootFrameObjectDomain(psdom)
        io = true
      }
      val inte = new TopSootInterpretation[JimpleMethod, params.type](params)
      params.interpretation = Some(inte)
      test(s"Jimple object analysis from input: ${methodName}") {
        try {          
          val env = Environment()
          if (!jmethod.body.getMethod.isStatic())
            env.addBinding("@this")
          for (i <- 0 until c.getMethodByName(methodName).getParameterCount())
            env.addBinding("@parameter"+i)
           for(l <- jmethod.locals )
            env.addBinding(l.getName)
         if (jmethod.body.getMethod.getReturnType() != VoidType.v())
            env.addBinding("@return")
            //          val env = Environment(jmethod.locals map { _.getName() } :_*)
          
          val parser = new PairSharingParser(env)
          val prop = parser.parseProperty(ps).get.toSet
          val input = parser.parseProperty("{(@this, @this)}").get.toSet
          val inputProp = if (jmethod.body.getMethod.isStatic()) 
      //                        params.domain.bottom(SootCFG.inputTypes(c.getMethodByName(methodName)))
                              psdom.bottom(SootCFG.inputTypes(c.getMethodByName(methodName)))
                              else 
                              psdom(input,Seq(c.getType))
//          val ann = jmethod.analyzeFromInput(params)(inputProp)
          val ann = jmethod.analyzeFromInput(params)(params.domain.bottom(SootCFG.inputTypes(c.getMethodByName(methodName))))
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
