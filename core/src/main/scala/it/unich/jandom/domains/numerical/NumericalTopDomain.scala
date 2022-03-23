/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.utils.numberext.RationalExt
import it.unich.scalafix.Box

/**
 * The most abstract numerical domain. It has a single top element for each dimension.
 * @author Gianluca Amato <gamato@unich.it>
 */
object NumericalTopDomain extends NumericalDomain {

  case class Property private[NumericalTopDomain] (val dimension: Int) extends NumericalProperty[Property] {
    require(dimension >= 0)

    type Domain = NumericalTopDomain.type

    def domain = NumericalTopDomain

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
      case other: Property if dimension == other.dimension => Some(0)
      case _ => None
    }
    def widening(that: Property) = this
    def union(that: Property) = this
    def narrowing(that: Property) = this
    def intersection(that: Property) = this
    def nonDeterministicAssignment(n: Int) = this
    def linearAssignment(n: Int, lf: LinearForm) = this
    def linearInequality(lf: LinearForm) = this
    def linearDisequality(lf: LinearForm) = this

    def minimize(lf: LinearForm) =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.NegativeInfinity
      else
        RationalExt(lf.known)

    def maximize(lf: LinearForm) =
      if (lf.homcoeffs.exists(!_.isZero))
        RationalExt.PositiveInfinity
      else
        RationalExt(lf.known)

    def frequency(lf: LinearForm) =
      if (lf.homcoeffs.exists(!_.isZero))
        Option.empty
      else
        Option(lf.known)

    def constraints = List()

    def isPolyhedral = true

    def addVariable() = new Property(dimension + 1)
    def delVariable(n: Int) = new Property(dimension - 1)
    def mapVariables(rho: Seq[Int]) = new Property(rho count { _ != -1 })

    def isEmpty = false
    def isTop = true
    def isBottom = true
    def bottom = this
    def top = this
    def mkString(vars: Seq[String]): String = "full"
  }

  val widenings = Seq(
    WideningDescription("default", "The trivial widening which just returns top.", Box.right[Property]))

  def top(n: Int) = Property(n)
  def bottom(n: Int) = Property(n)
}
