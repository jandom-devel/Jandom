/**
 * Copyright 2013 Gianluca Amato, Francesca Scozzari
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

import it.unich.sci.jandom.domains.numerical.NumericalDomain

import it.unich.sci.jandom.domains.numerical.NumericalProperty

/**
 * This is the class which implements the sum of two abstract properties.
 * @todo This is only a stub.
 * @author Gianluca Amato
 * @author Francesca Scozzari
 */
class Sum[Prop1 <: NumericalProperty[Prop1], Prop2 <: NumericalProperty[Prop2]](val p1: Prop1, val p2: Prop2)
  extends NumericalProperty[Sum[Prop1, Prop2]] {

  require(p1.dimension == p2.dimension)

  type Property = Sum[Prop1, Prop2]

  def union(that: Property): Property = {
    val q1 = p1 union that.p1
    val q2 = p2 union that.p2
    new Sum(q1, q2)
  }

  def widening(that: Property): Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def narrowing(that: Property): Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def intersection(that: Property): Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def nonDeterministicAssignment(n: Int): Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def linearAssignment(n: Int, coeff: Array[Double], known: Double): Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def linearInequality(coeff: Array[Double], known: Double): Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def linearDisequality(coeff: Array[Double], known: Double): Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def addDimension: Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def delDimension(n: Int): Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def mapDimensions(rho: Seq[Int]): Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def dimension: Int =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def isEmpty: Boolean =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def isFull: Boolean =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def empty: Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def full: Property =
    throw new IllegalAccessException("Unimplemented feature on Sum")

  def mkString(vars: IndexedSeq[String]): Seq[String] =
    throw new IllegalAccessException("Unimplemented feature on Sum")
  
  def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = 
    throw new IllegalAccessException("Unimplemented feature on Sum")
}

/**
 * This is the class for the sum of abstract domains.
 * @todo This is only a stub.
 * @author Gianluca Amato
 * @author Francesca Scozzari
 */
class SumDomain(val dom1: NumericalDomain, val dom2: NumericalDomain) extends NumericalDomain {
  
  type Property = Sum[dom1.Property, dom2.Property]
  
  def full(n: Int) = throw new IllegalAccessException("Unimplemented feature on Sum")
  
  def empty(n: Int) = throw new IllegalAccessException("Unimplemented feature on Sum")
}