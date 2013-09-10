/**
 * Copyright 2013 Francesca Scozzari <fscozzari@unich.it>
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

package it.unich.sci.jandom.ui

import soot.Scene
import it.unich.sci.jandom.targets.jvmsoot.BafMethod
import it.unich.sci.jandom.domains.numerical.NumericalDomain
import it.unich.sci.jandom.domains.objects.ObjectDomain
import it.unich.sci.jandom.targets.jvmsoot.ClassReachableAnalysis
import it.unich.sci.jandom.targets.jvmsoot.SootFrameDomain
import it.unich.sci.jandom.targets.jvmsoot.SootFrameNumericalDomain
import it.unich.sci.jandom.targets.jvmsoot.SootFrameObjectDomain
import it.unich.sci.jandom.domains.DimensionFiberedDomain
import it.unich.sci.jandom.targets.Parameters
import it.unich.sci.jandom.ppfactories.MemoizingFactory
import it.unich.sci.jandom.targets.jvmsoot.TopSootInterpretation
import it.unich.sci.jandom.targets.WideningScope
import it.unich.sci.jandom.targets.NarrowingStrategy
import it.unich.sci.jandom.ppfactories.DelayedWideningFactory
import it.unich.sci.jandom.widenings.DefaultWidening
import it.unich.sci.jandom.ppfactories.DelayedNarrowingFactory
import it.unich.sci.jandom.narrowings.NoNarrowing
import it.unich.sci.jandom.targets.jvmsoot.UnsupportedSootUnitException
import scala.swing.Dialog
import it.unich.sci.jandom.targets.jvmsoot.SootCFG
import soot.toolkits.graph.Block
import it.unich.sci.jandom.targets.jvmsoot.JimpleMethod
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.FileVisitResult
import java.nio.file.attribute.BasicFileAttributes
import scala.util.Try
import java.nio.file.Files
import it.unich.sci.jandom.ui.gui.SootEditorPane

/**
 * An output interface is a collection of methods for implementing an external interface.
 */
object OutputInterface {
  
  /**
   * Analyze a class using Baf. 
   * @param method the method to be analyzed
   * @param domain the abstract domain to be used (either numerical or object)
   * @param widening the widening strategy
   * @param narrowing the narrowing strategy
   * @param delay the widening delay
   * @param debug is true when the debug is active
   * @return a string with the program annotated with the analysis result
   */
  
  def analyze[T<:SootCFG[T, Block]](method: SootCFG[T, Block], domain: DimensionFiberedDomain, widening: WideningScope.Value,
		  	   narrowing: NarrowingStrategy.Value, delay:Int, debug: Boolean):String =  {
      try {
        val sootScene: Scene = Scene.v()
        val klassAnalysis = new ClassReachableAnalysis(sootScene)
  	    val sootDomain:SootFrameDomain = domain match {
  	  		case domain:NumericalDomain => new SootFrameNumericalDomain(domain)
  	  		case domain:ObjectDomain    => new SootFrameObjectDomain(domain,klassAnalysis)
        //case _ => None
        }
        val tMethod = method.asInstanceOf[T]
        val params = new Parameters[T] { val domain = sootDomain }
      
        params.wideningScope = widening
        params.narrowingStrategy = narrowing
        if (delay != 0) {
        	params.wideningFactory = DelayedWideningFactory(DefaultWidening, delay)
        }
        params.narrowingFactory = DelayedNarrowingFactory(NoNarrowing, 2)
   		if (debug) params.debugWriter = new java.io.StringWriter
   		params.wideningFactory = MemoizingFactory(tMethod)(params.wideningFactory)
   		params.narrowingFactory = MemoizingFactory(tMethod)(params.narrowingFactory)
   		val inte = new TopSootInterpretation[T, params.type](params)
   		params.interpretation = Some(inte)
   		val ann = tMethod.analyze(params)
   		tMethod.mkString(params)(ann)
      } catch {
        case e: UnsupportedSootUnitException =>
        	e.getMessage + " : " + e.unit + " Error in analysing bytecode"
        case e: Exception =>
            e.getMessage + " Error in parsing source code"
      }     
  }
  
  def analyze(klass: String, method: String, isBaf: Boolean, domain: DimensionFiberedDomain, widening: WideningScope.Value,
		  	   narrowing: NarrowingStrategy.Value, delay:Int, debug: Boolean):String = {
	val c = Scene.v().loadClassAndSupport(klass)
    c.setApplicationClass()
	if(isBaf)
	  analyze(new BafMethod(c.getMethodByName(method)),domain,widening, narrowing, delay, debug)
	else
	  analyze(new JimpleMethod(c.getMethodByName(method)),domain,widening, narrowing, delay, debug)  
  }
  
  
  private def getScene(file: String) = {
	  val scene = Scene.v()                     
	  val sootKlass = scene.loadClassAndSupport(file)
	  sootKlass.setApplicationClass()
	  scene
  }
  
  def unzipJar(dir:String, jarFile:String, destDir:String) = {
    val jar = new java.util.jar.JarFile(dir+"/"+jarFile);
    val enum = jar.entries();  
    while (enum.hasMoreElements()) {
    	val file = enum.nextElement(); //(java.util.jar.JarEntry)
    	val f = new java.io.File(destDir + java.io.File.separator + file.getName());
    	if (file.isDirectory()) { // if its a directory, create it
    		f.mkdir();
    	} else {
    		val is = jar.getInputStream(file); // get the input stream
    		val fos = new java.io.FileOutputStream(f);
    		while (is.available() > 0) {  // write contents of 'is' to 'fos'
    			fos.write(is.read());
    		}
    		fos.close();
    		is.close();
    		}
    }
  }
  
   
  def getMethods(file: String, klass:String) = {
    val scene = getScene(file)
    scene.loadClassAndSupport(klass).getMethods()
  }
  
  def getInput(file: String, klass:String, isBaf: Boolean,  method: String) = {
    
    
    
  }
  /*
  def getClasses(classPathField: String) = {
     val rootPath = Paths.get(classPathField)   
     val fileProcessor = new ClassFileVisitor(rootPath)
     if (Try(Files.walkFileTree(rootPath, fileProcessor)).isSuccess) {
        // these two lines are a mess because Scala Swing does not play well with Java 1.7
    	 fileProcessor.classNameList
      }
  }
  */
}
/*
class ClassFileVisitor(rootPath: Path) extends SimpleFileVisitor[Path] {
    private val privateClassNamesList = scala.collection.mutable.SortedSet[String]()
    def classNameList = privateClassNamesList.toSeq
    override def visitFile(aFile: Path, aAttrs: BasicFileAttributes): FileVisitResult = {
      val relativePath = rootPath.relativize(aFile)
      val className = (relativePath.head.toString /: relativePath.tail)(_ + "." + _.toString)
      if (className endsWith ".class")
        privateClassNamesList += className stripSuffix ".class"
      else if (className endsWith ".java")
        privateClassNamesList += className stripSuffix ".java"
      FileVisitResult.CONTINUE;
    }
  }
  */ 