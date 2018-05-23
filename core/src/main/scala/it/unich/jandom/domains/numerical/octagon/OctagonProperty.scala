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

  /**
    * Approximated with forget
    */
  def nonDeterministicAssignment(n: Int): Property = o.forget(Var(n + 1))

  /**
    * Dispatches a linearAssignment call to the handler for the
    * appropriate octagon exact abstract assignment operator (as
    * detailed in Mine' 2006 fig. 15 p. 35) or overapproximates using
    * the Box domain
    */
  def linearAssignment(j0 : Int, l: LinearForm): Property = {
    // TODO: Would it be a good idea to specially handle the cases with c = 0?
    val lf = l.padded(dimension + 1) // TODO: splain
    assert(lf.homcoeffs.size > j0, ""+lf + lf.homcoeffs + j0 + dimension)
    val zipped = lf.homcoeffs.zipWithIndex
    if (zipped.filter(_._1 != 0).size == 0) {
      // Case 1. {{ Vj0 <- c }}
      o.assign_vj0_gets_c(Var(j0 + 1), lf.known)
    } else if (zipped.filter(_._1 != 0).size == 1) {
      if (lf.homcoeffs(j0) == 1) {
        // Case 2. {{ Vj0 <- Vj0 + c }}
        if (lf.known == 0)
          o // vj <- vj + 0
        else
          o.assign_vj0_gets_vj0_plus_c(Var(j0 + 1), lf.known)
      } else if (lf.homcoeffs(j0) == -1) {
        // Case 4. {{ Vj0 <- - Vj0 + c }}
        // TODO OR NoT?
        if (lf.known == 0)
          o.assign_vj0_gets_minus_vj0(Var(j0 + 1)) // TODO: totally useless?
        else
          o.assign_vj0_gets_minus_vj0_plus_c(Var(j0 + 1), lf.known)
      } else {
        // 1 coeff; possibly case 3. 5 or 7
        val coeff = zipped.filter(_._1 != 0).head._1
        val i0 = zipped.filter(_._1 != 0).head._2
        if (coeff == 1) {
          // Case 3. {{ Vj0 <- Vi0 + c }}
          o.assign_vj0_gets_vi0_plus_c(Var(j0 + 1), Var(i0 + 1), lf.known)
        } else if (coeff == -1) {
          // Case ??? {{ Vj0 <- -Vi0 }}
          if (lf.known == 0)
            o.assign_vj0_gets_minus_vi0(Var(j0 + 1), Var(i0 + 1)) // TODO: totally useless?
          else
            o.assign_vj0_gets_minus_vi0_plus_c(Var(j0 + 1), Var(i0 + 1), lf.known)
        } else {
          // coeff is not -1, +1, use box fallback
          domain.fromBox(toBox.linearAssignment(j0, l))
        }
      }
    } else {
      // Use fallback: More than 2 non-zero coefficients
      domain.fromBox(toBox.linearAssignment(j0, l))
    }
  }

  /**
    * Dispatches a linearInequality call to the handler for the
    * appropriate octagon exact abstract test operator (as detailed in
    * Mine' 2006 fig. 20 . 42) or overapproximates using the Box
    * domain
    */
  def linearInequality(lf: LinearForm): Property = {
    // lf : c + a1 v1 + a2 v2 + ... <= 0
    if (o.isBottom)
      this // TODO: Is this useless? If o is bottom its own transfer
           // functions should be okay already
    else {
      val zipped = lf.homcoeffs.zipWithIndex
      if (zipped.filter(_._1 != 0).size == 0)
        if (lf.known <= 0)
          this // The solution to 0x1 + 0x2 + ... + 0xn + c <= 0 is "anything" if c <= 0
        else // lf.known > 0
          bottom
            // The solution to 0x1 + 0x2 + ... + 0xn + c <= 0 is _|_ if c > 0
      else if (zipped.filter(_._1 != 0).size == 1) {
        val coeff = zipped.filter(_._1 != 0).head._1
        val j0 = zipped.filter(_._1 != 0).head._2
        if (coeff == 1)
          // Case 1. {{ Vj0 + c <= 0 ? }}
          o.test_vj0_plus_c_le_0(Var(j0 + 1), lf.known)
        else if (coeff == -1)
          // Case 2. {{ -Vj0 + c <= 0 ? }}
          o.test_minus_vj0_plus_c_le_0(Var(j0 + 1), lf.known)
        else
          // Use box fallback: {{ k Vj0 + c <= 0 ? }} for k != 1, -1
          domain.fromBox(toBox.linearInequality(lf))
      } else if (zipped.filter(_._1 != 0).size == 2) {
        val coeffj0 = zipped.filter(_._1 != 0).head._1
        val j0 = zipped.filter(_._1 != 0).head._2
        val coeffi0 = zipped.filter(_._1 != 0).tail.head._1
        val i0 = zipped.filter(_._1 != 0).tail.head._2
        if (coeffj0 == 1 & coeffi0 == -1)
          // Case 3.  {{ Vj0 - Vi0 + c <= 0 ? }}
          o.test_vj0_minus_vi0_plus_c_le_0(Var(j0 + 1), Var(i0 + 1), lf.known)
        else if (coeffj0 == -1 & coeffi0 == 1)
          // Case 3 bis.  (with Vj0 <-> Vi0)
          o.test_vj0_minus_vi0_plus_c_le_0(Var(i0 + 1), Var(j0 + 1), lf.known)
        else if (coeffj0 == 1 & coeffi0 == 1)
          // Case 4.  {{ Vj0 + Vi0 + c <= 0 ? }}
          o.test_vj0_plus_vi0_le_c(Var(i0 + 1), Var(j0 + 1), lf.known)
        else if (coeffj0 == -1 & coeffi0 == -1)
          // Case 5. {{ -Vj0 - Vi0 + c <= 0 ? }}
          o.test_minus_vj0_minus_vi0_plus_c_le_0(Var(j0 + 1), Var(i0 + 1), lf.known)
        else
          // Use box fallback:  {{ k Vj0 + k' Vi0 + c <= 0 ? }} for k, k' not in 1, -1
          domain.fromBox(toBox.linearInequality(lf))
      } else {
        // Use fallback: More than 2 non-zero coefficients
        domain.fromBox(toBox.linearInequality(lf))
      }
    }
  }

  def linearDisequality(lf: LinearForm): Property = ???

  def toBox : BoxRationalDomain#Property = ???

  def minimize(lf: LinearForm) = ???

  def maximize(lf: LinearForm) = ???

  def frequency(lf: LinearForm) = ???

  def constraints : Seq[LinearForm] =
    if (o.isBottom)
      Seq()
    else {(for {
          i <- o.dimension.allVars.flatMap(x => Seq(x.posForm, x.negForm)) : Seq[SignedVarIdx];
          j <- o.dimension.allVars.flatMap(x => Seq(x.posForm, x.negForm)) : Seq[SignedVarIdx];
          c : RationalExt = o.get_ineq_vi_minus_vj_leq_c(i,j).get
          if (!c.isInfinity) // +oo if there is no constraint (Mine 2006 p. 7)
          if (i != j) // Don't care about x - x = 0
          arri = Array
          .fill[Rational](o.dimension.octagonDimToInt + 1)(Rational(0))
          .updated(i.toVar.i, Rational(i.coeff))
          arr = arri
          .updated(j.toVar.i, arri(j.toVar.i) - j.coeff)
          .updated(0, -c.value) // vi - vj <= c  ===>  vi -vj - c <= 0
        } yield {
          if (j.bar == i)
            // Normalize those 2v <= 2c constraints that happen when
            // you have ij positive and negative form of a single
            // var. You can remove this and it's still technically
            // correct ("the best kind of correct", according to an
            // internet meme).
            LinearForm(arr : _*) / 2
          else
            LinearForm(arr : _*)
        }
        ).distinct  // We remove duplicates, since "some octagonal constraints
                    // have two different encodings...in the DBM" [Mine 2006 p. 9]
                    // This normalization step is also technically superfluous.
    }

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
    "[ " +// constraints
      it.unich.jandom.domains.numerical.Inequalities.constraintsToInequalities(constraints).map(_.mkString(vars)).mkString("; ") + " ]"
    }
  }
}
