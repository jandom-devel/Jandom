/**
 * Copyright 2013, 2016 Jandom Team
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

package it.unich.jandom.domains.numerical

import it.unich.jandom.domains.DomainTransformation
import it.unich.jandom.domains.WideningDescription
import it.unich.scalafix.Box

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
 * @author Gianluca Amato <gianluca.amato@unich.it>
 * @author Francesca Scozzari <francesca.scozzari@unich.it>
 */
class ProductDomain[D1 <: NumericalDomain, D2 <: NumericalDomain](val dom1: D1, val dom2: D2)(
    implicit val dom1Todom2: DomainTransformation[D1, D2], val dom2Todom1: DomainTransformation[D2, D1]) extends NumericalDomain {

  val widenings = {
    for (w1 <- dom1.widenings; w2 <- dom2.widenings)
      yield WideningDescription(s"${w1.name} X ${w2.name}", s"Component-wise combination of the two widenings.",
      Box { (a: Property, b: Property) =>
        new Property(w1.box(a.p1, b.p1), w2.box(a.p2, b.p2))
      })
  }

  private val d12 = dom1Todom2(dom1, dom2)
  private val d21 = dom2Todom1(dom2, dom1)

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
      if (x1.isEmpty || x2.isEmpty)
        new Property(x1.bottom, x2.bottom)
      else {
        val y1 = x1.intersection(d21(x2))
        val y2 = x2.intersection(d12(x1))
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

    def linearAssignment(n: Int, lf: LinearForm): Property = {
      val q1 = p1.linearAssignment(n, lf)
      val q2 = p2.linearAssignment(n, lf)
      reduce(q1, q2)
    }

    def linearInequality(lf: LinearForm): Property = {
      val q1 = p1.linearInequality(lf)
      val q2 = p2.linearInequality(lf)
      reduce(q1, q2)
    }

    def linearDisequality(lf: LinearForm): Property = {
      val q1 = p1.linearDisequality(lf)
      val q2 = p2.linearDisequality(lf)
      reduce(q1, q2)
    }

    def minimize(lf: LinearForm) = {
      val q1 = p1.minimize(lf)
      val q2 = p2.minimize(lf)
      q1 max q2
    }

    def maximize(lf: LinearForm) = {
      val q1 = p1.maximize(lf)
      val q2 = p2.maximize(lf)
      q1 min q2
    }

    def frequency(lf: LinearForm) = {
      // This could be made more precise when the concrete domain is integer
      p1.frequency(lf) match {
        case v @ Some(c) => v
        case None => p2.frequency(lf)
      }
    }

    def constraints = p1.constraints ++ p2.constraints

    def isPolyhedral = p1.isPolyhedral && p2.isPolyhedral

    def addVariable(): Property =
      new Property(p1.addVariable(), p2.addVariable())

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

    def bottom: Property = domain.bottom(dimension)

    def top: Property = domain.top(dimension)

    def mkString(vars: Seq[String]): String = {
      if (isEmpty)
        "empty"
      else if (isTop)
        "full"
      else
        p1.mkString(vars) + " / " + p2.mkString(vars)
    }

    def tryCompareTo[B >: Property](that: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
      that match {
        case that: Property =>
          if (this.isBottom && that.isBottom)
            Option(0)
          else if (this.isBottom)
            Option(-1)
          else if (that.isBottom)
            Option(1)
          else if (this.isTop && that.isTop)
            Option(0)
          else if (this.isTop)
            Option(1)
          else if (that.isTop)
            Option(-1)
          else if (p1 == this.p1 && p2 == that.p2)
            Option(0)
          else
            Option.empty
        case _ => Option.empty
      }
    }
  }
}
