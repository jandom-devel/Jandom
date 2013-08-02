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

package it.unich.sci.jandom.domains.numerical

/**
 * This is the class which implements the product of two basic numerical properties. It is not a
 * real reduced product, but a cartesian product with some reduction given by transformation
 * funtions.
 * @todo This is only a stub.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 *
 */
class Product[Prop1 <: NumericalProperty[Prop1], Prop2 <: NumericalProperty[Prop2]](val p1: Prop1, val p2: Prop2)
  extends NumericalProperty[Product[Prop1, Prop2]] {

  require(p1.dimension == p2.dimension)

  type Property = Product[Prop1, Prop2]

  def this(pair:(Prop1,Prop2)) = {
    this(pair._1,pair._2)
  }

  def reduce(x1: Prop1, x2: Prop2) = {
    if(x1.isEmpty && x2.isEmpty)
    	(x1,x2)
    if(x1.isEmpty)
	    new Product(x1,x2.empty)
	if(x2.isEmpty)
      new Product(x1.empty,x2)
    (x1,x2)
    // to be done.....
  }

  def union(that: Property): Property = {
    val q1 = p1 union that.p1
    val q2 = p2 union that.p2
    new Product(reduce(q1, q2))
  }

  def widening(that: Property): Property =
    new Product(this.p1 widening that.p1, this.p2 widening that.p2)

  def narrowing(that: Property): Property =
    new Product(this.p1 narrowing that.p1, this.p2 narrowing that.p2)

  def intersection(that: Property): Property = {
   	val q1 = p1 intersection that.p1
    val q2 = p2 intersection that.p2
    new Product(reduce(q1, q2))
  }

  def nonDeterministicAssignment(n: Int): Property = {
    val q1 = p1.nonDeterministicAssignment(n)
    val q2 = p2.nonDeterministicAssignment(n)
    new Product(reduce(q1,q2))
  }

  def linearAssignment(n: Int, coeff: Array[Double], known: Double): Property = {
    val q1 = p1.linearAssignment(n, coeff, known)
    val q2 = p2.linearAssignment(n, coeff, known)
    new Product(reduce(q1,q2))
  }

  def linearInequality(coeff: Array[Double], known: Double): Property = {
    val q1 = p1.linearInequality(coeff, known)
    val q2 = p2.linearInequality(coeff, known)
    new Product(reduce(q1,q2))
  }

  def linearDisequality(coeff: Array[Double], known: Double): Property = {
    val q1 = p1.linearDisequality(coeff, known)
    val q2 = p2.linearDisequality(coeff, known)
    new Product(reduce(q1,q2))
  }

  def minimize(coeff: Array[Double], known: Double): Double =
    throw new IllegalAccessException("Unimplemented feature on Product")

  def maximize(coeff: Array[Double], known: Double): Double =
    throw new IllegalAccessException("Unimplemented feature on Product")

  def frequency(coeff: Array[Double], known: Double): Some[Double] =
    throw new IllegalAccessException("Unimplemented feature on Product")

  def addDimension: Property =
    new Product(p1.addDimension, p2.addDimension)

  def delDimension(n: Int): Property =
    new Product(p1.delDimension(n), p2.delDimension(n))

  def mapDimensions(rho: Seq[Int]): Property = {
    val q1 = p1.mapDimensions(rho)
    val q2 = p2.mapDimensions(rho)
    new Product(reduce(q1,q2))
  }

  def dimension: Int = p1.dimension

  def isEmpty: Boolean =
  	p1.isEmpty && p2.isEmpty

  def isFull: Boolean =
  	p1.isFull && p2.isFull

  def empty: Property =
    new Product(p1.empty, p2.empty)

  def full: Property =
  	new Product(p1.full, p2.full)

  def mkString(vars: IndexedSeq[String]): Seq[String] = {
    if (isEmpty)
      Seq("[void]")
    else {
      val s1 = p1.mkString(vars)
      val s2 = p2.mkString(vars)
      for (i <- 0 until dimension) yield "("+s1(i)+" , "+s2(i)+")"
    }
}

  def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
     other match {
       case Product(_,_) =>
       	val comp1 =  p1.tryCompareTo(other.p1)
       	val comp2 =  p2.tryCompareTo(other.p2)
       	if(comp1==comp2)
       		comp1
       		else
        None
     	}
}
}
/**
 * This is the class for the product of abstract domains.
 * @todo This is only a stub.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
class ProductDomain(val dom1: NumericalDomain, val dom2: NumericalDomain) extends NumericalDomain {

  type Property = Product[dom1.Property, dom2.Property]

  def full(n: Int) =
    new Product(dom1.full(n), dom2.full(n))

  def empty(n: Int) =
    throw new IllegalAccessException("Unimplemented feature on Product")
}
