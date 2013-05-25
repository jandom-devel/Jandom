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

import it.unich.sci.jandom.domains.numerical.NumericalProperty
import scala.collection.immutable.BitSet
import it.unich.sci.jandom.domains.numerical.NumericalDomain
import it.unich.sci.jandom.targets.linearcondition.AtomicCond
import it.unich.sci.jandom.targets.LinearForm
import it.unich.sci.jandom.targets.linearcondition.LinearCond
import soot.PrimType

/**
 * This class only represent numerical information using the NumericalProperty class N.
 */
case class ObjectNumericalProperty[N <: NumericalProperty[N]](val prop: N, val numerical: BitSet) extends ObjectProperty[ObjectNumericalProperty[N]] {

  def tryCompareTo[B >: ObjectNumericalProperty[N]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
    other match {
      case other: ObjectNumericalProperty[N] => prop tryCompareTo other.prop
      case _ => None
    }

  def addVariable(tpe: soot.Type) = ObjectNumericalProperty(prop.addDimension, if (tpe.isInstanceOf[soot.IntegerType]) numerical + size else numerical)
  def delVariable = ObjectNumericalProperty(prop.delDimension(), numerical - (size - 1))

  def size = prop.dimension

  def variable(n: Int) = n

  def evalConstant(const: Int) = ObjectNumericalProperty(prop.addDimension.constantAssignment(size, const), numerical + size)
  def evalNull = ObjectNumericalProperty(prop.addDimension, numerical)
  def evalNew = evalNull
  def evalVariable(v: Int) = if (numerical contains v)
    ObjectNumericalProperty(prop.addDimension.variableAssignment(size, v), numerical + size)
  else
    ObjectNumericalProperty(prop.addDimension, numerical)
  def evalField(v: Int, f: Int) = evalNull

  def assignConstant(v: Int, c: Int) = new ObjectNumericalProperty(prop.constantAssignment(v, c), numerical + v)
  def assignVariable(dst: Int) =
    if (numerical contains (size - 1))
      new ObjectNumericalProperty(prop.variableAssignment(dst, size - 1).delDimension(size - 1), numerical + dst - (size - 1))
    else
      this
  def assignField(dst: Int, fieldNum: Int) = this

  override def evalAdd = ObjectNumericalProperty(prop.variableAdd().delDimension(), numerical - (size - 1))
  override def evalSub = ObjectNumericalProperty(prop.variableSub().delDimension(), numerical - (size - 1))
  override def evalMul = ObjectNumericalProperty(prop.variableMul().delDimension(), numerical - (size - 1))
  override def evalDiv = ObjectNumericalProperty(prop.variableDiv().delDimension(), numerical - (size - 1))
  override def evalRem = ObjectNumericalProperty(prop.variableRem().delDimension(), numerical - (size - 1))
  override def evalShl = ObjectNumericalProperty(prop.variableShl().delDimension(), numerical - (size - 1))
  override def evalShr = ObjectNumericalProperty(prop.variableShr().delDimension(), numerical - (size - 1))
  override def evalUshr = ObjectNumericalProperty(prop.variableUshr().delDimension(), numerical - (size - 1))

  def evalBinOp = ObjectNumericalProperty(prop.delDimension().delDimension().addDimension, numerical)
  def evalNeg = ObjectNumericalProperty(prop.variableNeg().delDimension(), numerical)
  def evalLength = ObjectNumericalProperty(prop, numerical + (size - 1))

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
    coeffs(size) = 1.0
    val lf = LinearForm(coeffs)
    val tbranch = ObjectNumericalProperty(AtomicCond(lf, op).analyze(prop), numerical - (size - 1) - (size - 2))
    val fbranch = ObjectNumericalProperty(AtomicCond(lf, AtomicCond.ComparisonOperators.opposite(op)).analyze(prop), numerical - (size - 1) - (size - 2))
    (tbranch, fbranch)
  }

  def testGt = testComp(AtomicCond.ComparisonOperators.GT)
  def testGe = testComp(AtomicCond.ComparisonOperators.GTE)
  def testLe = testComp(AtomicCond.ComparisonOperators.LTE)
  def testLt = testComp(AtomicCond.ComparisonOperators.LT)
  def testEq = testComp(AtomicCond.ComparisonOperators.EQ)
  def testNe = testComp(AtomicCond.ComparisonOperators.NEQ)

  def testLinearCondition(lc: LinearCond) = (
    ObjectNumericalProperty(lc.analyze(prop), numerical),
    ObjectNumericalProperty(lc.opposite.analyze(prop), numerical))

  def union(that: ObjectNumericalProperty[N]) = {
    ObjectNumericalProperty(prop union that.prop, numerical intersect that.numerical)
  }

  def intersection(that: ObjectNumericalProperty[N]) = {
    if (numerical == that.numerical)
      new ObjectNumericalProperty(prop intersection that.prop, numerical)
    else
      throw new IllegalArgumentException("Intersection between incompatible states")
  }

  def widening(that: ObjectNumericalProperty[N]) = {
    if (numerical == that.numerical)
      new ObjectNumericalProperty(prop widening that.prop, numerical)
    else
      throw new IllegalArgumentException("Widening between incompatible states")
  }

  def narrowing(that: ObjectNumericalProperty[N]) = {
    if (numerical == that.numerical)
      new ObjectNumericalProperty(prop narrowing that.prop, numerical)
    else
      throw new IllegalArgumentException("Narrowing between incompatible states")
  }

  def isEmpty = prop.isEmpty

  def mkString(vars: IndexedSeq[String]) = prop.mkString(vars) :+ numerical.mkString(" ")
}

class ObjectNumericalDomain(val numdom: NumericalDomain) extends ObjectDomain {
  type Property = ObjectNumericalProperty[numdom.Property]

  private def typesToNumerical(roots: Seq[ soot.Type ]): BitSet = {
     val indices = for ( (tpe,index) <- roots.zipWithIndex; if tpe.isInstanceOf[PrimType] ) yield index
     BitSet(indices: _*)
  }

  def top( roots: Seq[soot.Type] ) = ObjectNumericalProperty( numdom.full(roots.size), typesToNumerical(roots))
  def bottom (roots: Seq[soot.Type]) = ObjectNumericalProperty(numdom.empty(roots.size), typesToNumerical(roots))
  def initial(roots: Seq[soot.Type]) = top(roots)
  def apply(prop: numdom.Property) = new ObjectNumericalProperty(prop, BitSet(0 until prop.dimension: _*))
}
