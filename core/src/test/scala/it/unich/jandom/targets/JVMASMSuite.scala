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

import scala.collection.JavaConverters._
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode
import org.scalatest.FunSuite
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.targets.jvmasm.{AsmMethod, JVMEnvFixedFrame, JVMEnvFixedFrameDomain, UnsupportedASMInsnException}

class JVMASMSuite extends FunSuite {
  val BoxDouble = BoxDoubleDomain()

  test("simple method analysis") {
    val is = getClass().getResourceAsStream("/javatest/SimpleTest.class")
    val cr = new ClassReader(is)
    val node = new ClassNode()
    cr.accept(node, ClassReader.SKIP_DEBUG)
    val methodList = node.methods.asScala
    val method = new AsmMethod(methodList.find(_.name == "conditional").get)
    val params = new Parameters[AsmMethod] {
      val domain = new JVMEnvFixedFrameDomain(BoxDouble)
    }
    try {
      val ann = method.analyze(params)
      val result = new JVMEnvFixedFrame(params.domain, 2, BoxDouble(Array(0.0, 2.0), Array(0.0, 2.0)))
      assertResult(result) { ann(method.lastPP.get) }
    } catch {
      case e: UnsupportedASMInsnException =>
        fail(e.toString)
    } finally {
      is.close
    }
  }
}
