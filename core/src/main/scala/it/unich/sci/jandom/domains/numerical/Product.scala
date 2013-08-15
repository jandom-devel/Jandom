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

import it.unich.sci.jandom.domains.DomainTransformation

/**
 * This is the class for the product of abstract domains.
 * @todo This is only a stub.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
 abstract class ProductDomain extends NumericalDomain {
  val dom1: NumericalDomain
  val dom2: NumericalDomain
  val dom1Todom2: DomainTransformation[dom1.Property, dom2.Property]
  val dom2Todom1: DomainTransformation[dom2.Property, dom1.Property]

  type Property = ProductProperty

  def top(n: Int) =
    new Property(dom1.top(n), dom2.top(n))

  def bottom(n: Int) =
    new Property(dom1.bottom(n), dom2.bottom(n))


  /**
   * This is the class which implements the product of two basic numerical properties. It is not a
   * real reduced product, but a cartesian product with some reduction given by transformation
   * funtions.
   * @author Gianluca Amato <gamato@unich.it>
   * @author Francesca Scozzari <fscozzari@unich.it>
   *
   */
  class ProductProperty(val p1: dom1.Property, val p2: dom2.Property)
    extends NumericalProperty[ProductProperty] {

    //type Property = ProductProperty

    require(p1.dimension == p2.dimension)
/*
    def apply(x: dom1.Property, y: dom2.Property):Property = {
    if(x.isEmpty || y.isEmpty)
    	 empty
    else
    	new ProductProperty(x,y)
  }
*/
    /*
  def this(pair:(dom1.Property,dom2.Property)) = {
    this(pair._1,pair._2)
  }
*/

    def reduce(x1: dom1.Property, x2: dom2.Property): Property = {
      if (x1.isEmpty && x2.isEmpty)
        this
      else if (x1.isEmpty)
        new Property(x1, x2.empty)
      else if (x2.isEmpty)
        new Property(x1.empty, x2)
      else {
        val y1=x1.intersection(dom2Todom1.apply(x2))
        val y2=x2.intersection(dom1Todom2.apply(x1))

//        val z1=y1.intersection(dom2Todom1.apply(y2))
//        val z2=y2.intersection(dom1Todom2.apply(y1))

        new Property(y1, y2)
      }
      // to be done.....
    }

    def union(that: Property): Property = {
      val q1 = p1 union that.p1
      val q2 = p2 union that.p2
      reduce(q1, q2)
    }

    /*
     * We do not reduce since it may prevent termination
     */
    def widening(that: Property): Property =
      new Property(this.p1 widening that.p1, this.p2 widening that.p2)

    /*
     * We do not reduce since it may prevent termination
     */
    def narrowing(that: Property): Property =
      new Property(this.p1 narrowing that.p1, this.p2 narrowing that.p2)

    def intersection(that: Property): Property = {
      val q1 = p1 intersection that.p1
      val q2 = p2 intersection that.p2
      reduce(q1, q2)
    }

    def nonDeterministicAssignment(n: Int): Property = {
      val q1 = p1.nonDeterministicAssignment(n)
      val q2 = p2.nonDeterministicAssignment(n)
      reduce(q1, q2)
    }

    def linearAssignment(n: Int, coeff: Array[Double], known: Double): Property = {
      val q1 = p1.linearAssignment(n, coeff, known)
      val q2 = p2.linearAssignment(n, coeff, known)
      reduce(q1, q2)
    }

    def linearInequality(coeff: Array[Double], known: Double): Property = {
      val q1 = p1.linearInequality(coeff, known)
      val q2 = p2.linearInequality(coeff, known)
      reduce(q1, q2)
    }

    def linearDisequality(coeff: Array[Double], known: Double): Property = {
      val q1 = p1.linearDisequality(coeff, known)
      val q2 = p2.linearDisequality(coeff, known)
      reduce(q1, q2)
    }

    def minimize(coeff: Array[Double], known: Double): Double = {
    	val q1=p1.minimize(coeff, known)
    	val q2=p2.minimize(coeff, known)
    	q1 max q2
    }

    def maximize(coeff: Array[Double], known: Double): Double = {
    	val q1=p1.maximize(coeff, known)
    	val q2=p2.maximize(coeff, known)
    	q1 min q2
    }

    def frequency(coeff: Array[Double], known: Double): Option[Double] =
   		if(p1.frequency(coeff, known) == p2.frequency(coeff, known))
    		  p1.frequency(coeff, known)
    		  else None

    def addVariable: Property =
      new Property(p1.addVariable, p2.addVariable)

    def delVariable(n: Int): Property =
      new Property(p1.delVariable(n), p2.delVariable(n))

    def mapDimensions(rho: Seq[Int]): Property = {
      val q1 = p1.mapDimensions(rho)
      val q2 = p2.mapDimensions(rho)
      reduce(q1, q2)
    }

    def dimension: Int = p1.dimension

    def isEmpty: Boolean =
      p1.isEmpty || p2.isEmpty

    def isFull: Boolean =
      p1.isFull && p2.isFull

    def empty: Property =
      	ProductDomain.this.bottom(dimension)

    def full: Property =
       	ProductDomain.this.top(dimension)

    def mkString(vars: IndexedSeq[String]): Seq[String] = {
      if (isEmpty)
        Seq("[voidProduct]")
      else {
        val s1 = p1.mkString(vars)
        val s2 = p2.mkString(vars)
        for (i <- 0 until dimension) yield "(" + s1(i) + " , " + s2(i) + ")"
      }
    }

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
      other match {
        case other: Property => {
          val c1 = p1.tryCompareTo(other.p1)
          val c2 = p2.tryCompareTo(other.p2)
          if (c1 == 0)
            c2
          else if (c2 == 0)
            c1
          else if (c1 == c2)
            c1
          else
            None
        }
 //       case other:dom1.Property => (p1.intersection(ProductDomain.this.dom2Todom1(p2))) tryCompareTo other
 //       case other:dom2.Property => (p2.intersection(ProductDomain.this.dom1Todom2(p1))) tryCompareTo other
        case _ => None
      }
    }
  }

}


