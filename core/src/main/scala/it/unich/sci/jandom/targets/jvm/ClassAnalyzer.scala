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

package it.unich.sci.jandom.targets.jvm

import scala.collection.mutable.HashMap

import org.objectweb.asm.{ ClassReader, ClassVisitor, Opcodes }

import it.unich.sci.jandom.targets.Target

/**
 * This analyzes a class file.
 * @author Gianluca Amato
 *
 */
class ClassAnalyzer(val cr: ClassReader) extends Target {
  type ProgramPoint = Int
  type Annotation[Property] = HashMap[ProgramPoint, Property]
  type Tgt = ClassAnalyzer

  var namex: String = null
  cr.accept(new Extractor(),0)

  class Extractor extends ClassVisitor(Opcodes.ASM4) {
    override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) {
      namex = name
    }
  }

  def getAnnotation[Property]: Annotation[Property] = new Annotation[Property]
  def size = 0
  def analyze(params: Parameters): Annotation[params.Property] = getAnnotation[params.Property]
  override def toString = namex
}