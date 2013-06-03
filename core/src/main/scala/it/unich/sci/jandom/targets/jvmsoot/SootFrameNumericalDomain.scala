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

package it.unich.sci.jandom.targets.jvmsoot

import scala.collection.immutable.BitSet
import scala.collection.immutable.Stack
import it.unich.sci.jandom.domains.numerical.NumericalDomain
import it.unich.sci.jandom.targets.LinearForm
import it.unich.sci.jandom.targets.linearcondition.AtomicCond
import it.unich.sci.jandom.targets.linearcondition.LinearCond
import soot._
import soot.baf.WordType

class SootFrameNumericalDomain(val numdom: NumericalDomain) extends SootFrameDomain {

  def top(vars: Seq[Type]) = Property(numdom.full(vars.size), Stack(vars: _*))
  def bottom(vars: Seq[Type]) = Property(numdom.empty(vars.size), Stack(vars: _*))
  def initial(vars: Seq[Type]) = top(vars)

/*  private def canonicalType(tpe: Type) = tpe match {
    case _ : BooleanType => IntType.v()
    case _ : ByteType => IntType.v()
    case _ : ShortType => IntType.v()
    case x  => x
  }*/

  def apply(prop: numdom.Property, tpe: Type): Property = Property(prop, Stack((for (i <- 0 until prop.dimension) yield tpe): _*))
  def apply(prop: numdom.Property, tpes: Seq[Type]): Property = apply(prop, Stack(tpes: _*))

  case class Property(val prop: numdom.Property, val vars: Stack[Type]) extends SootFrameProperty[Property] {

    def size = prop.dimension

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
      other match {
        case other: Property => prop tryCompareTo other.prop
        case _ => None
      }

    private def addVariable(tpe: Type) = Property(prop.addDimension, vars.push(tpe))
    private def delVariable = Property(prop.delDimension(), vars.pop)

    def evalConstant(const: Int) = Property(prop.addDimension.constantAssignment(size, const), vars.push(IntType.v()))
    def evalNull = addVariable(NullType.v())
    def evalNew(tpe: Type) = addVariable(tpe)
    def evalLocal(v: Int) = {
      if (vars(v).isInstanceOf[PrimType] || vars(v).isInstanceOf[WordType])
        Property(prop.addDimension.variableAssignment(size, v), vars.push(vars(v)))
      else
        addVariable(vars(v))
    }
    def evalField(v: Int, f: SootField) = addVariable(f.getType())

    def assignLocal(dst: Int) = {
       if (vars(dst).isInstanceOf[PrimType] || vars(dst).isInstanceOf[WordType])
        new Property(prop.variableAssignment(dst, size - 1).delDimension(), vars.pop)
      else
        delVariable
    }

    def assignField(dst: Int, f: SootField) = delVariable

    def evalAdd = Property(prop.variableAdd().delDimension(), vars.pop)
    def evalSub = Property(prop.variableSub().delDimension(), vars.pop)
    def evalMul = Property(prop.variableMul().delDimension(), vars.pop)
    def evalDiv = Property(prop.variableDiv().delDimension(), vars.pop)
    def evalRem = Property(prop.variableRem().delDimension(), vars.pop)
    def evalShl = Property(prop.variableShl().delDimension(), vars.pop)
    def evalShr = Property(prop.variableShr().delDimension(), vars.pop)
    def evalUshr = Property(prop.variableUshr().delDimension(), vars.pop)

    def evalBinOp = Property(prop.delDimension().delDimension().addDimension, vars.pop)
    def evalNeg = Property(prop.variableNeg().delDimension(), vars)
    def evalLength = addVariable(IntType.v())

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
      val tbranch = Property(AtomicCond(lf, op).analyze(prop).delDimension().delDimension(), vars.pop.pop)
      val fbranch = Property(AtomicCond(lf, AtomicCond.ComparisonOperators.opposite(op)).analyze(prop).delDimension().delDimension(), vars.pop.pop)
      (tbranch, fbranch)
    }

    def testGt = testComp(AtomicCond.ComparisonOperators.GT)
    def testGe = testComp(AtomicCond.ComparisonOperators.GTE)
    def testLe = testComp(AtomicCond.ComparisonOperators.LTE)
    def testLt = testComp(AtomicCond.ComparisonOperators.LT)
    def testEq = testComp(AtomicCond.ComparisonOperators.EQ)
    def testNe = testComp(AtomicCond.ComparisonOperators.NEQ)

    def testLinearCondition(lc: LinearCond) = (
      Property(lc.analyze(prop), vars), Property(lc.opposite.analyze(prop),vars)
    )

    def union(that: Property) = Property(prop union that.prop, vars)

    def intersection(that: Property) = Property(prop intersection that.prop, vars)

    def widening(that: Property) = Property(prop widening that.prop, vars)

    def narrowing(that: Property) = Property(prop widening that.prop, vars)

    def restrict(n: Int) = Property(
       (0 until size-n).foldLeft(prop) { (x: numdom.Property, i: Int) => x.delDimension(0) },
        vars.drop(n)
    )

    def connect(p: Property, common: Int) =
      Property(prop.connect(p.prop, common), vars.drop(common) ++ p.vars)

    def isEmpty = prop.isEmpty

  /*  def isCompatibleWith(that: Property) =
    	prop == that.prop &&
    	vars.size == that.vars.size &&
    	(vars map canonicalType) == (that.vars map canonicalType)*/

    def mkString(vars: IndexedSeq[String]) = prop.mkString(vars) // :+ ("types: "+this.vars.toString)
  }
}
