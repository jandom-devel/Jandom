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
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.sci.jandom.domains.numerical

import it.unich.sci.jandom.domains.DomainTransformation

/**
 * This class implements the reduced product of two abstract domains. It is not a
 * real reduced product, but a cartesian product with some reduction given by transformation
 * funtions.
 * @tparam D1 the class of the first abstract domain
 * @tparam D2 the class of the second abstract domain
 * @param dom1 first abstract domain
 * @param dom2 second abstract domain
 * @param dom1Todom2 domain transformer from D1 to D2
 * @param dom2Todom1 domain transformer from D1 to D2
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
final class ProductDomain[D1 <: NumericalDomain, D2 <: NumericalDomain](val dom1: D1, val dom2: D2)(
      implicit val dom1Todom2: DomainTransformation[D1,D2], val dom2Todom1: DomainTransformation[D2,D1]) extends NumericalDomain {

  private val d12 = dom1Todom2(dom1,dom2)
  private val d21 = dom2Todom1(dom2,dom1)

  def top(n: Int) =
    new Property(dom1.top(n), dom2.top(n))

  def bottom(n: Int) =
    new Property(dom1.bottom(n), dom2.bottom(n))

  /**
   * This class represents the reduced product of two base numerical properties.
   * @author Gianluca Amato <gamato@unich.it>
   * @author Francesca Scozzari <fscozzari@unich.it>
   */
  class Property(val p1: dom1.Property, val p2: dom2.Property) extends NumericalProperty[Property] {

    require(p1.dimension == p2.dimension)

    type Domain = ProductDomain.this.type

    def domain = ProductDomain.this

    def reduce(x1: dom1.Property, x2: dom2.Property): Property = {
      if (x1.isEmpty && x2.isEmpty)
        this
      else if (x1.isEmpty)
        new Property(x1, x2.bottom)
      else if (x2.isEmpty)
        new Property(x1.bottom, x2)
      else {
        val y1=x1.intersection(d21(x2))
        val y2=x2.intersection(d12(x1))

        new Property(y1, y2)
      }
    }

    def union(that: Property): Property = {
      val q1 = p1 union that.p1
      val q2 = p2 union that.p2
      reduce(q1, q2)
    }

    def widening(that: Property): Property =
      // We do not reduce since it may prevent termination
      new Property(this.p1 widening that.p1, this.p2 widening that.p2)

    def narrowing(that: Property): Property =
      // We do not reduce since it may prevent termination
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

    def linearAssignment(n: Int, lf: LinearForm[Double]): Property = {
      val q1 = p1.linearAssignment(n, lf)
      val q2 = p2.linearAssignment(n, lf)
      reduce(q1, q2)
    }

    def linearInequality(lf: LinearForm[Double]): Property = {
      val q1 = p1.linearInequality(lf)
      val q2 = p2.linearInequality(lf)
      reduce(q1, q2)
    }

    def linearDisequality(lf: LinearForm[Double]): Property = {
      val q1 = p1.linearDisequality(lf)
      val q2 = p2.linearDisequality(lf)
      reduce(q1, q2)
    }

    def minimize(lf: LinearForm[Double]): Double = {
    	val q1=p1.minimize(lf)
    	val q2=p2.minimize(lf)
    	q1 max q2
    }

    def maximize(lf: LinearForm[Double]): Double = {
    	val q1=p1.maximize(lf)
    	val q2=p2.maximize(lf)
    	q1 min q2
    }

    def frequency(lf: LinearForm[Double]): Option[Double] = {
    	p1.frequency(lf) match {
    	  case v@ Some(c) => v
    	  case None => p2.frequency(lf)
    	}
    }

    def addVariable: Property =
      new Property(p1.addVariable, p2.addVariable)

    def delVariable(n: Int): Property =
      new Property(p1.delVariable(n), p2.delVariable(n))

    def mapVariables(rho: Seq[Int]): Property = {
      val q1 = p1.mapVariables(rho)
      val q2 = p2.mapVariables(rho)
      reduce(q1, q2)
    }

    def dimension: Int = p1.dimension

    def isEmpty = p1.isEmpty || p2.isEmpty

    def isTop = p1.isTop && p2.isTop

    def isBottom = isEmpty || (p1.isBottom && p2.isBottom)

    def bottom: Property = ProductDomain.this.bottom(dimension)

    def top: Property = ProductDomain.this.top(dimension)

    def mkString(vars: Seq[String]): String = {
      if (isEmpty)
        "empty"
      else if (isTop)
        "full"
      else
        p1.mkString(vars) + " / " + p2.mkString(vars)
    }

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
      other match {
        case other: Property => {
          val c1 = p1.tryCompareTo(other.p1)
          val c2 = p2.tryCompareTo(other.p2)
          if (c1 == Some(0))
            c2
          else if (c2 == Some(0))
            c1
          else if (c1 == c2)
            c1
          else
            None
        }
        case _ => None
      }
    }
  }
}
