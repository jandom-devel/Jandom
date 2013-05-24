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

package it.unich.sci.jandom.domains.objects

import it.unich.sci.jandom.domains.AbstractProperty
import it.unich.sci.jandom.domains.numerical.NumericalProperty
import it.unich.sci.jandom.targets.linearcondition.LinearCond
import it.unich.sci.jandom.targets.LinearForm

/**
 * This is the base trait for all properties of objects.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait ObjectProperty[Property <: ObjectProperty[Property]] extends AbstractProperty[Property] {
  this: Property =>

  type Variable = Int

  /**
   * Returns number of variables in the root scope
   */
  def size: Int

  def delVariable: Property

  def evalConstant(c: Int): Property
  def evalNull: Property
  def evalNew: Property
  def evalVariable(v: Variable): Property
  def evalField(v: Variable, f: Int): Property

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

  /**
   * Assign a variable to another variable.
   * @param v the destination variable.
   */
  def assignVariable(dst: Variable): Property
  def assignField(dst: Variable, fieldNum: Int): Property
  def test: (Property, Property)
  def testGt: (Property, Property)
  def testGe: (Property, Property)
  def testLt: (Property, Property)
  def testLe: (Property, Property)
  def testEq: (Property, Property)
  def testNe: (Property, Property)

  def testLinearCondition(lf: LinearCond): (Property, Property)

  def isTop = false
  def isBottom = false

  override def toString: String = "[ " + (mkString(for (i <- 0 until size) yield "v" + i)).mkString(" , ") + " ]"
}
