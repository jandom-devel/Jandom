/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

import scala.collection.immutable.Stack

import it.unich.sci.jandom.domains.AbstractDomain
import it.unich.sci.jandom.domains.AbstractProperty
import it.unich.sci.jandom.targets.linearcondition.LinearCond

import soot._

/**
 * This is the base trait domain for the analysis of methods using Soot. It represents
 * an abstract frame with locals and stack.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait SootFrameDomain extends AbstractDomain {
  type Property <: SootFrameProperty[Property]

  trait SootFrameProperty[Property <: SootFrameProperty[Property]] extends AbstractProperty[Property] {
    this: Property =>

    def roots: IndexedSeq[Local]

    def size: Int

    def stacksize: Int = size - roots.size

    def evalConstant(c: Int): Property
    def evalNull: Property
    def evalNew(tpe: Type): Property
    def evalLocal(l: Local): Property
    def evalField(l: Local, f: SootField): Property

    def evalAdd: Property
    def evalSub: Property
    def evalMul: Property
    def evalDiv: Property
    def evalRem: Property
    def evalShl: Property
    def evalShr: Property
    def evalUshr: Property
    def evalBinOp: Property
    def evalNeg: Property
    def evalLength: Property

    def evalGt: Property
    def evalGe: Property
    def evalLt: Property
    def evalLe: Property
    def evalEq: Property
    def evalNe: Property

    def assignLocal(l: Local): Property
    def assignField(l: Local, f: SootField): Property

    def test: (Property, Property)
    def testGt: (Property, Property)
    def testGe: (Property, Property)
    def testLt: (Property, Property)
    def testLe: (Property, Property)
    def testEq: (Property, Property)
    def testNe: (Property, Property)

    def testLinearCondition(lf: LinearCond): (Property, Property)

    override def toString = mkString((roots map { _.getName() }) ++
      (for (i <- 0 until stacksize) yield "s" + i)).mkString(", ")

    def isTop = false
    def isBottom = false
  }

  def initial: Property
  def top(stack: Stack[Type]): Property
  def bottom(stack: Stack[Type]): Property
}
