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
 
  val jimpleTestDir = java.nio.file.Paths.get(System.getProperty("user.dir"),"src","test","resources","jimpleTest")
  scene.setSootClassPath(jimpleTestDir.toString)
  val c = scene.loadClassAndSupport("SimpleTest")
  val classAnalysis = new ClassReachableAnalysis(scene)
  
  jimplePairSharingTests()
  
  def jimplePairSharingTests() {
    val jimplePairSharingTests: Seq[(String, Set[UP[Int]])] = Seq(
      "sequential" -> Set(),
      "conditional" -> Set(),
      "loop" -> Set(),
      "nested" -> Set(),
      "longassignment" -> Set(),
      "topologicalorder" -> Set(),
      "complexif" -> Set(),
      "objcreation" -> Set(UP(0, 0), UP(0, 5), UP(1, 1), UP(1, 5), UP(5, 6), UP(6, 7), UP(2, 2), UP(0, 1), UP(0, 6), UP(6, 6), UP(5, 7), UP(3, 6), UP(3, 5),
        UP(1, 3), UP(1, 8), UP(2, 4), UP(0, 7), UP(5, 8), UP(3, 7), UP(0, 3), UP(1, 7), UP(7, 8), UP(8, 8), UP(0, 8), UP(4, 4), UP(5, 5), UP(7, 7), UP(3, 3),
        UP(3, 8), UP(1, 6), UP(6, 8)),
      "classrefinement" -> Set(UP(0, 0), UP(1, 1), UP(5, 6), UP(2, 2), UP(0, 1), UP(2, 3), UP(6, 6), UP(4, 5), UP(3, 6), UP(3, 5), UP(2, 4), UP(3, 4),
        UP(2, 5), UP(4, 4), UP(5, 5), UP(3, 3)),
      "class_parametric" -> Set(UP(0, 0), UP(0, 5), UP(1, 1), UP(1, 5), UP(2, 2), UP(0, 1), UP(0, 2), UP(3, 4), UP(1, 2), UP(2, 5), UP(4, 4), UP(5, 5), UP(3, 3)))

    for ((methodName, ps) <- jimplePairSharingTests) {
      val method = new JimpleMethod(c.getMethodByName(methodName))
      val params = new Parameters[JimpleMethod] {
        val domain = new SootFrameObjectDomain(PairSharingDomain,classAnalysis)
        io = true
        //debugWriter = new java.io.PrintWriter(System.err)
      }
      val inte = new TopSootInterpretation[JimpleMethod, params.type](params)
      params.interpretation = Some(inte)
      test(s"Jimple object analysis: ${methodName}") {
        try {
          val ann = method.analyze(params)
          assert(ann(method.lastPP.get).prop === PairSharingDomain.Property(ps, method.locals.size + method.body.getMethod().getParameterCount()))
        } finally {
          params.debugWriter.flush()
        }
      }
    }
  }
  
  def createClass() {
  
  // resolve dependencies
  scene.loadClassAndSupport("java.lang.Object")
  scene.loadClassAndSupport("java.lang.System")
  
  // create the class "public class TestClass extends Object"
  val testClass = new SootClass("TestClass", Modifier.PUBLIC)
  testClass.setSuperclass(scene.getSootClass("java.lang.Object"))
  scene.addClass(testClass)
   
  // create the method "public static void main(String[])"
  val parameters =  new ArrayList[Type]()
  parameters.add({ArrayType.v(RefType.v("java.lang.String"), 1)})
  
  val mainMethod = new SootMethod("main", parameters, VoidType.v(), Modifier.PUBLIC | Modifier.STATIC)
 
  testClass.addMethod(mainMethod)
  
  // create the method body
  val body = Jimple.v().newBody(mainMethod)           
  mainMethod.setActiveBody(body)
  val units = body.getUnits()
  
  // add a local variable "java.lang.String l0"
  val arg = Jimple.v().newLocal("l0", ArrayType.v(RefType.v("java.lang.String"), 1))
  body.getLocals().add(arg)
                
  // add local "java.io.printStream tmpRef"
  val tmpRef = Jimple.v().newLocal("tmpRef", RefType.v("java.io.PrintStream"))
  body.getLocals().add(tmpRef)
                
  // add "l0 = @parameter0"
  units.add(Jimple.v().newIdentityStmt(arg, 
        Jimple.v().newParameterRef(ArrayType.v(RefType.v("java.lang.String"), 1), 0)))
            
  // add "tmpRef = java.lang.System.out"
  units.add(Jimple.v().newAssignStmt(tmpRef, Jimple.v().newStaticFieldRef(
       Scene.v().getField("<java.lang.System: java.io.PrintStream out>").makeRef())));
            
  // insert "tmpRef.println("Hello world!")"
  val toCall = Scene.v().getMethod("<java.io.PrintStream: void println(java.lang.String)>");
  units.add(Jimple.v().newInvokeStmt(Jimple.v().newVirtualInvokeExpr(tmpRef, toCall.makeRef(), StringConstant.v("Hello world!"))))
            
  // insert "return"
  units.add(Jimple.v().newReturnVoidStmt())
  
  // set the abstract domain 
  val classAnalysis = new ClassReachableAnalysis(scene)
  val c = testClass
  
  }
}

// If you just want to get a Soot scene from a set of Jimple files, 
// you can add the respective directories to Soot's classpath, 
// run soot.Main.main() and the classes should be read in automatically.