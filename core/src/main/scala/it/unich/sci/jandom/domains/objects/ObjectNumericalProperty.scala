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

/**
 * This class only represent numerical information using the NumericalProperty class N.
 */
case class ObjectNumericalProperty[N <: NumericalProperty[N]](val prop: N, val numerical: BitSet) extends ObjectProperty[ObjectNumericalProperty[N]] {

  def tryCompareTo[B >: ObjectNumericalProperty[N]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
    other match {
      case other: ObjectNumericalProperty[N] => prop tryCompareTo other.prop
      case _ => None
    }

  def size = prop.dimension

  def variable(n: Int) = n

  def evalConstant(const: Int) = ObjectNumericalProperty(prop.addDimension.constantAssignment(size, const), numerical + size)
  def evalNull = ObjectNumericalProperty(prop.addDimension, numerical)
  def evalNew = evalNull
  def evalVariable(v: Int) = if (numerical contains v)
    ObjectNumericalProperty(prop.addDimension.variableAdd(size, v), numerical + size)
  else
    ObjectNumericalProperty(prop.addDimension, numerical)
  def evalLength = evalNull
  def evalField(v: Int, f: Int) = evalNull

  def assignConstant(v: Int, c: Int) = new ObjectNumericalProperty(prop.constantAssignment(v, c), numerical + v)
  def assignVariable(dst: Int) =
    if (numerical contains (size-1))
      new ObjectNumericalProperty(prop.variableAssignment(dst, size-1).delDimension(size-1), numerical + dst - (size-1))
    else
      this
  def assignField(dst: Int, fieldNum: Int) = this

  def evalAdd = ObjectNumericalProperty(prop.variableAdd().delDimension(), numerical - (size - 1))
  def evalSub = ObjectNumericalProperty(prop.variableSub().delDimension(), numerical - (size - 1))
  def evalMul = ObjectNumericalProperty(prop.variableMul().delDimension(), numerical - (size - 1))
  def evalDiv = ObjectNumericalProperty(prop.variableDiv().delDimension(), numerical - (size - 1))
  def evalRem = ObjectNumericalProperty(prop.variableRem().delDimension(), numerical - (size - 1))
  def evalShl = ObjectNumericalProperty(prop.variableShl().delDimension(), numerical - (size - 1))
  def evalShr = ObjectNumericalProperty(prop.variableShr().delDimension(), numerical - (size - 1))
  def evalUshr = ObjectNumericalProperty(prop.variableUshr().delDimension(), numerical - (size - 1))
  def evalBinOp = ObjectNumericalProperty(prop.delDimension().delDimension().addDimension, numerical)
  def evalNeg = ObjectNumericalProperty(prop.variableNeg().delDimension(), numerical)

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
  def top(n: Int) = ObjectNumericalProperty(numdom.full(n), BitSet())
  def bottom(n: Int) = ObjectNumericalProperty(numdom.empty(n), BitSet())
  def initial(n: Int) = top(n)
  def apply(prop: numdom.Property) = new ObjectNumericalProperty(prop, BitSet(0 until prop.dimension: _*))
}
