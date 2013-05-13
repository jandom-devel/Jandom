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

package it.unich.sci.jandom.domains

import it.unich.sci.jandom.domains.numerical.NumericalProperty
import it.unich.sci.jandom.domains.objects.ObjectProperty

/**
 * @author Gianluca Amato <gamato@unich.it>
 */
case class SimpleObjectNumericalProperty[P1 <: NumericalProperty[P1], P2 <: ObjectProperty[P2]] (val num: P1, val obj: P2)
  extends NumericalProperty[SimpleObjectNumericalProperty[P1,P2]] with ObjectProperty[SimpleObjectNumericalProperty[P1,P2]] {

  this: SimpleObjectNumericalProperty[P1,P2] =>

  def tryCompareTo[B >: SimpleObjectNumericalProperty[P1,P2]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: SimpleObjectNumericalProperty[P1,P2] =>
       val left = num.tryCompareTo(other.num)
       val right = obj.tryCompareTo(other.obj)
       (left, right) match {
         case (Some(0), x) => x
         case (x, Some(0)) => x
         case (Some(x), Some(y)) if x==y => Some(x)
         case _ => None
       }
    case _ => None
  }

  def widening(that: SimpleObjectNumericalProperty[P1,P2]) = new SimpleObjectNumericalProperty(num widening that.num, obj widening that.obj)
  def union(that: SimpleObjectNumericalProperty[P1,P2]) = new SimpleObjectNumericalProperty(num union that.num, obj union that.obj)
  def narrowing(that: SimpleObjectNumericalProperty[P1,P2]) = new SimpleObjectNumericalProperty(num narrowing that.num, obj narrowing that.obj)
  def intersection(that: SimpleObjectNumericalProperty[P1,P2]) = new SimpleObjectNumericalProperty(num intersection that.num, obj intersection that.obj)

  def nonDeterministicAssignment(n: Int) = new SimpleObjectNumericalProperty(num.nonDeterministicAssignment(n), obj)
  def linearAssignment(n: Int, coeff: Array[Double], known: Double) = new SimpleObjectNumericalProperty(num.linearAssignment(n, coeff, known), obj)
  def linearInequality(coeff: Array[Double], known: Double) = new SimpleObjectNumericalProperty(num.linearInequality(coeff, known), obj)
  def linearDisequality(coeff: Array[Double], known: Double) = new SimpleObjectNumericalProperty(num.linearDisequality(coeff, known), obj)
  def addDimension = new SimpleObjectNumericalProperty(num.addDimension, obj)
  def delDimension(n: Int = dimension - 1) = new SimpleObjectNumericalProperty(num.delDimension(n), obj)
  def mapDimensions(rho: Seq[Int]) = new SimpleObjectNumericalProperty(num.mapDimensions(rho), obj)
  def isEmpty = num.isEmpty
  def isFull = num.isFull
  def empty = new SimpleObjectNumericalProperty(num.empty,obj)
  def full = new SimpleObjectNumericalProperty(num.full,obj)
  def mkString(vars: IndexedSeq[String]): Seq[String] = num.mkString(vars) ++ obj.mkString(vars)
  def dimension = num.dimension
}
