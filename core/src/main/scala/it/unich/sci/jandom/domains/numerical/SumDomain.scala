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
 * This is the class for the sum of two abstract domains.
 * @todo This is only a stub.
 * @param dom1 first numerical domain
 * @param dom2 second numerical domain
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
class SumDomain(val dom1: NumericalDomain, val dom2: NumericalDomain) extends NumericalDomain {

  /**
   * This class represens a single property in the sum domain, which is the Minkowski sum
   * of `p1` and `p2`.
   * @param p1 property of the first domain
   * @param p2 property of the second domain
   */
  class Property(val p1: dom1.Property, val p2: dom2.Property) extends NumericalProperty[Property] {

    require(p1.dimension == p2.dimension)

    def union(that: Property): Property = {
      val q1 = p1 union that.p1
      val q2 = p2 union that.p2
      new Property(q1, q2)
    }

    def widening(that: Property): Property = ???

    def narrowing(that: Property): Property = ???

    def intersection(that: Property): Property = ???

    def nonDeterministicAssignment(n: Int): Property = ???

    def linearAssignment(n: Int, lf: LinearForm[Double]): Property = ???

    def linearInequality(lf: LinearForm[Double]): Property = ???

    def linearDisequality(lf: LinearForm[Double]): Property = ???

    def minimize(lf: LinearForm[Double]): Double = ???

    def maximize(lf: LinearForm[Double]): Double = ???

    def frequency(lf: LinearForm[Double]): Some[Double] = ???

    def addVariable: Property = ???

    def delVariable(n: Int): Property = ???

    def mapVariables(rho: Seq[Int]): Property = ???

    def dimension = ???

    def isTop = ???

    def isEmpty = ???

    def isBottom = ???

    def bottom: Property = ???

    def top: Property = ???

    def mkString(vars: Seq[String]) = ???

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = ???
  }

  def top(n: Int) = ???

  def bottom(n: Int) = ???
}

