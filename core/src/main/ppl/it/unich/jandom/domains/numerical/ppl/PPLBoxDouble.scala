/**
 * Copyright 2013, 2016 Gianluca Amato
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

package it.unich.jandom.domains.numerical.ppl

import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.utils.numberext.RationalExt
import parma_polyhedra_library._
import spire.math.Rational

/**
 * This is an element of the domain `PPLBoxDoubleDomain`
 * This is essentially a wrapper transforming methods of `Double_Box` to methods of `NumericalProperty`.
 * We clone objects in order to get an immutable class.
 * @author Gianluca Amato <gamato@unich.it>
 */
final class PPLBoxDouble(val pplbox: Double_Box) extends NumericalProperty[PPLBoxDouble] {

  type Domain = PPLBoxDoubleDomain

  def domain = PPLBoxDoubleDomain()

  def widening(that: PPLBoxDouble): PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)
    newpplbox.upper_bound_assign(that.pplbox)
    newpplbox.CC76_widening_assign(pplbox, null)
    new PPLBoxDouble(newpplbox)
  }

  /**
   * The CC76 widening for two boxes.
   * @param that the abstract object to be widened with `this`. `that` is NOT assumed to be bigger than `this`.
   * @return the widening of the two abstract properties.
   */
  def CC76Widening(that: PPLBoxDouble): PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)
    newpplbox.upper_bound_assign(that.pplbox)
    newpplbox.CC76_widening_assign(pplbox, null)
    new PPLBoxDouble(newpplbox)
  }

  def narrowing(that: PPLBoxDouble): PPLBoxDouble = {
    val newpplbox = new Double_Box(that.pplbox)
    newpplbox.CC76_narrowing_assign(pplbox)
    new PPLBoxDouble(newpplbox)
  }

  def union(that: PPLBoxDouble): PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)
    val x = new Double_Box(pplbox.space_dimension(), Degenerate_Element.EMPTY)
    newpplbox.upper_bound_assign(that.pplbox)
    new PPLBoxDouble(newpplbox)
  }

  def intersection(that: PPLBoxDouble): PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)
    newpplbox.intersection_assign(that.pplbox)
    new PPLBoxDouble(newpplbox)
  }

  def nonDeterministicAssignment(n: Int): PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)
    newpplbox.unconstrain_space_dimension(new Variable(n))
    new PPLBoxDouble(newpplbox)
  }

  def linearAssignment(n: Int, lf: LinearForm): PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)
    val (le, den) = PPLUtils.toPPLLinearExpression(lf)
    newpplbox.affine_image(new Variable(n), le, den)
    new PPLBoxDouble(newpplbox)
  }

  def linearInequality(lf: LinearForm): PPLBoxDouble = {
    val (le, _) = PPLUtils.toPPLLinearExpression(lf)
    val newpplbox = new Double_Box(pplbox)
    newpplbox.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
    new PPLBoxDouble(newpplbox)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   */
  def linearDisequality(lf: LinearForm): PPLBoxDouble = {
    val (le, _) = PPLUtils.toPPLLinearExpression(lf)
    val newpplbox1 = new Double_Box(pplbox)
    val newpplbox2 = new Double_Box(pplbox)
    newpplbox1.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
    newpplbox2.refine_with_constraint(new Constraint(le, Relation_Symbol.GREATER_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
    newpplbox1.upper_bound_assign_if_exact(newpplbox2)
    new PPLBoxDouble(newpplbox1)
  }

  def minimize(lf: LinearForm) = {
    if (isEmpty) {
      if (lf.homcoeffs.forall(_.isZero))
        RationalExt(lf.known)
      else
        RationalExt.PositiveInfinity
    } else {
      val (le, den) = PPLUtils.toPPLLinearExpression(lf)
      val exact = new By_Reference[java.lang.Boolean](false)
      val val_n = new Coefficient(0)
      val val_d = new Coefficient(0)
      val result = pplbox.minimize(le, val_n, val_d, exact)
      if (!result)
        RationalExt.NegativeInfinity
      else
        RationalExt(val_n.getBigInteger, val_d.getBigInteger multiply den.getBigInteger)
    }
  }

  def maximize(lf: LinearForm) = {
    if (isEmpty) {
      if (lf.homcoeffs.forall(_ == 0.0))
        RationalExt(lf.known)
      else
        RationalExt.NegativeInfinity
    } else {
      val (le, den) = PPLUtils.toPPLLinearExpression(lf)
      val exact = new By_Reference[java.lang.Boolean](false)
      val val_n = new Coefficient(0)
      val val_d = new Coefficient(0)
      val result = pplbox.maximize(le, val_n, val_d, exact)
      if (!result)
        RationalExt.PositiveInfinity
      else
        RationalExt(val_n.getBigInteger, val_d.getBigInteger multiply den.getBigInteger)
    }
  }

  def frequency(lf: LinearForm) = {
    if (isEmpty) {
      if (lf.homcoeffs.forall(_ == 0.0))
        Option(lf.known)
      else
        Option.empty
    } else {
      val (le, den) = PPLUtils.toPPLLinearExpression(lf)
      val freq_n = new Coefficient(0)
      val freq_d = new Coefficient(0)
      val val_n = new Coefficient(0)
      val val_d = new Coefficient(0)
      val result = pplbox.frequency(le, freq_n, freq_d, val_n, val_d)
      if (!result)
        Option.empty
      else
        Option(Rational(val_n.getBigInteger, val_d.getBigInteger multiply den.getBigInteger))
    }
  }

  def constraints = {
    import scala.collection.JavaConversions._

    val cs = pplbox.constraints()
    cs flatMap PPLUtils.fromPPLConstraint
  }

  def isPolyhedral = {
    import scala.collection.JavaConversions._
    val cs = pplbox.constraints()
    cs forall PPLUtils.isRepresentableAsLinearForms
  }

  def addVariable: PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)
    newpplbox.add_space_dimensions_and_embed(1)
    new PPLBoxDouble(newpplbox)
  }

  def delVariable(n: Int): PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)
    val dims = new Variables_Set
    dims.add(new Variable(n))
    newpplbox.remove_space_dimensions(dims)
    new PPLBoxDouble(newpplbox)
  }

  def mapVariables(rho: Seq[Int]) = {
    val newpplbox = new Double_Box(pplbox)
    newpplbox.map_space_dimensions(PPLUtils.sequenceToPartialFunction(rho))
    new PPLBoxDouble(newpplbox)
  }

  def dimension: Int = pplbox.space_dimension.toInt

  def isEmpty: Boolean = pplbox.is_empty

  def isTop: Boolean = pplbox.is_universe

  def isBottom = isEmpty

  def bottom = domain.bottom(pplbox.space_dimension.toInt)

  def top = domain.top(pplbox.space_dimension.toInt)

  def tryCompareTo[B >: PPLBoxDouble](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: PPLBoxDouble =>
      if (pplbox == other.pplbox)
        Option(0)
      else if (pplbox strictly_contains other.pplbox)
        Option(1)
      else if (other.pplbox strictly_contains pplbox)
        Option(-1)
      else
        Option.empty
    case _ => Option.empty
  }

  override def equals(other: Any): Boolean = other match {
    case other: PPLBoxDouble => pplbox.equals(other.pplbox)
    case _ => false
  }

  override def hashCode: Int = pplbox.hashCode

  def mkString(vars: Seq[String]): String = PPLUtils.constraintsToString(pplbox.minimized_constraints(), vars)
}
