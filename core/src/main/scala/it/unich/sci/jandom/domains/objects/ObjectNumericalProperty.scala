/**
 * Copyright 2013 amato
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

import scala.collection.immutable.BitSet

import it.unich.sci.jandom.domains.numerical.NumericalDomain
import it.unich.sci.jandom.domains.numerical.NumericalProperty
import it.unich.sci.jandom.targets.LinearForm
import it.unich.sci.jandom.targets.linearcondition.AtomicCond
import it.unich.sci.jandom.targets.linearcondition.LinearCond
import soot._

class ObjectNumericalDomain(val numdom: NumericalDomain, val roots: IndexedSeq[Local]) extends ObjectDomain {

  private val numericalSeq = for ((local, index) <- roots.zipWithIndex; if local.getType().isInstanceOf[PrimType]) yield index
  val numerical = BitSet(numericalSeq: _*)
  val localMap: Map[Local, Int] = roots.zipWithIndex.toMap

  def top(stacksize: Int = 0) = Property(numdom.full(roots.size + stacksize))
  def bottom(stacksize: Int = 0) = Property(numdom.empty(roots.size + stacksize))
  def initial = top(0)
  def apply(prop: numdom.Property) = new Property(prop)

  case class Property(val prop: numdom.Property) extends ObjectProperty[Property] {

    def roots = ObjectNumericalDomain.this.roots

    def size = prop.dimension

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
      other match {
        case other: Property => prop tryCompareTo other.prop
        case _ => None
      }

    private def addVariable = Property(prop.addDimension)
    private def delVariable = Property(prop.delDimension())

    def evalConstant(const: Int) = Property(prop.addDimension.constantAssignment(size, const))
    def evalNull = addVariable
    def evalNew = addVariable
    def evalLocal(l: Local) = {
      val v = localMap(l)
      if (numerical contains v)
        Property(prop.addDimension.variableAssignment(size, v))
      else
        Property(prop.addDimension)
    }
    def evalField(l: Local, f: SootField) = addVariable

    def assignLocal(l: Local) = {
      val dst = localMap(l)
      if (numerical contains  dst)
        new Property(prop.variableAssignment(dst, size - 1).delDimension())
      else
        this
    }

    def assignField(l: Local, f: SootField) = delVariable

    def evalAdd = Property(prop.variableAdd().delDimension())
    def evalSub = Property(prop.variableSub().delDimension())
    def evalMul = Property(prop.variableMul().delDimension())
    def evalDiv = Property(prop.variableDiv().delDimension())
    def evalRem = Property(prop.variableRem().delDimension())
    def evalShl = Property(prop.variableShl().delDimension())
    def evalShr = Property(prop.variableShr().delDimension())
    def evalUshr = Property(prop.variableUshr().delDimension())

    def evalBinOp = Property(prop.delDimension().delDimension().addDimension)
    def evalNeg = Property(prop.variableNeg().delDimension())
    def evalLength = addVariable

    def evalGt = delVariable
    def evalGe = delVariable
    def evalLt = delVariable
    def evalLe = delVariable
    def evalEq = delVariable
    def evalNe = delVariable

    def test = {
      val dropped = delVariable
      (dropped, dropped)
    }

    private def testComp(op: AtomicCond.ComparisonOperators.Value) = {
      import AtomicCond.ComparisonOperators._
      val coeffs = Array.fill(size + 1)(0.0)
      coeffs(size - 1) = 1.0
      coeffs(size) = -1.0
      val lf = LinearForm(coeffs)
      val tbranch = Property(AtomicCond(lf, op).analyze(prop).delDimension().delDimension())
      val fbranch = Property(AtomicCond(lf, AtomicCond.ComparisonOperators.opposite(op)).analyze(prop).delDimension().delDimension())
      (tbranch, fbranch)
    }

    def testGt = testComp(AtomicCond.ComparisonOperators.GT)
    def testGe = testComp(AtomicCond.ComparisonOperators.GTE)
    def testLe = testComp(AtomicCond.ComparisonOperators.LTE)
    def testLt = testComp(AtomicCond.ComparisonOperators.LT)
    def testEq = testComp(AtomicCond.ComparisonOperators.EQ)
    def testNe = testComp(AtomicCond.ComparisonOperators.NEQ)

    def testLinearCondition(lc: LinearCond) = (
      Property(lc.analyze(prop)), Property(lc.opposite.analyze(prop))
     )

    def union(that: Property) = Property(prop union that.prop)

    def intersection(that: Property) = Property(prop intersection that.prop)

    def widening(that: Property) = Property(prop widening that.prop)

    def narrowing(that: Property) = Property(prop widening that.prop)

    def isEmpty = prop.isEmpty

    def mkString(vars: IndexedSeq[String]) = prop.mkString(vars) :+ ("dimension: " + size)
  }

}
