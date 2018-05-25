/**
  * Copyright 2018 Filippo Sestini, Tobia Tesan
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

package it.unich.jandom.domains.numerical.octagon
import  it.unich.jandom.domains.numerical.OctagonDomain
import  it.unich.jandom.domains.numerical.BoxRationalDomain
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.utils.numberext.RationalExt
import spire.math.Rational
import it.unich.jandom.utils.numberext.StaticIField
import scala.language.implicitConversions
import math.PartiallyOrdered

/**
  * This class adapts a NumericalProperty interface to the Octagon
  * interface, which exposes transfer functions and attributes found
  * in Mine 2006 Fig. 15, Fig. 20, etc; moreover, it uses the Boxes
  * domain as a fallback overapproximation.
  *
  * There is no actual octagon-related logic besides dispatching, nor
  * any additional state mantained herein.
  */
case class OctagonProperty
  (o : Octagon[RationalExt])
  (implicit ifield : StaticIField[RationalExt], octDomain : OctagonDomain, boxDomain : BoxRationalDomain)
    extends NumericalProperty[OctagonProperty] {

  implicit def octagonToProperty (o : Octagon[RationalExt]) : OctagonProperty = OctagonProperty(o)

  type Domain = OctagonDomain
  type Property = OctagonProperty

  def domain = octDomain


  def dimension = o.dimension.octagonDimToInt

  def widening(that : Property) = o.widening(that.o)

  def narrowing(that : Property) = o.narrowing(that.o)

  override def union(that: Property): Property = o.union(that.o)

  override def intersection(that: Property): Property = o.intersection(that.o)

  def nonDeterministicAssignment(n: Int): Property = ???

  def linearAssignment(j0 : Int, l: LinearForm): Property = ???

  def linearInequality(lf: LinearForm): Property = ???

  def linearDisequality(lf: LinearForm): Property = ???

  def toBox : BoxRationalDomain#Property = ???

  def minimize(lf: LinearForm) = ???

  def maximize(lf: LinearForm) = ???

  def frequency(lf: LinearForm) = ???

  def constraints : Seq[LinearForm] = ???

  def isPolyhedral = true

  def addVariable = ??? // TODO

  def delVariable(n: Int) = ??? // TODO

  def mapVariables(rho: Seq[Int]) = ??? // TODO

  def isEmpty = isBottom

  def isTop = o.isTop

  def isBottom = o.isBottom

  def bottom = domain.bottom(this.dimension)

  def top = domain.top(this.dimension)

  override def hashCode = 123456 + o.hashCode

  def tryCompareTo[B >: OctagonProperty](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
    other match {
      case OctagonProperty(othero) =>
        (this.o.tryCompareTo(othero))
      case _ => None
    }

  def mkString(vars: Seq[String]): String = {
    if (isTop) {
      "[ T ]"
    } else if (isBottom) {
      "[ _|_ ]"
      } else {
    "[ " + constraints + " ]"
    }
  }
}
