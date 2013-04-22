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

package it.unich.sci.jandom.targets

import java.io.FileInputStream

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.tree.MethodNode
import org.scalatest.FunSuite

import it.unich.sci.jandom.domains.PPLCPolyhedron
import it.unich.sci.jandom.targets.jvm.JVMEnvFixedFrameDomain
import it.unich.sci.jandom.targets.jvm.Method
import it.unich.sci.jandom.targets.jvm.UnsupportedASMByteCodeException

class JVMSuite extends FunSuite {
  import scala.collection.JavaConversions.asScalaBuffer

  test("simple method analysis") {
    val is = new FileInputStream("examples/Java/SimpleTest.class")
    val cr = new ClassReader(is)
    val node = new ClassNode()
    cr.accept(node, ClassReader.SKIP_DEBUG)
    val methodList = node.methods.asInstanceOf[java.util.List[MethodNode]]
    val method = new Method(methodList.find(_.name == "loop").get)
    val params = new Parameters(method) {
      val domain = new JVMEnvFixedFrameDomain(PPLCPolyhedron)
    }
    println(method.toString)
    try {
      val ann = method.analyze(params)
      println(method.mkString(ann))
    } catch {
      case e: UnsupportedASMByteCodeException =>
        println(e.node)
    }

    is.close
  }
}
