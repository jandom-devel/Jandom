/**
 * Copyright 2013 Gianluca Amato
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

package it.unich.sci.jandom.domains.numerical.ppl

import it.unich.sci.jandom.domains.numerical.LinearForm
import it.unich.sci.jandom.domains.numerical.NumericalDomain
import it.unich.sci.jandom.domains.numerical.NumericalProperty
import parma_polyhedra_library._
import it.unich.sci.jandom.domains.CachedTopBottom

/**
 * This is the universal PPL numerical property. It encapsulate an object of the PPL library and
 * use a `PPLDomain` as a proxy to call the right method.
 *
 * @constructor creates a new PPLProperty object
 * @tparam PPLNativeProperty is the PPL class implementing the abstract property, such as Double_Box,
 * Octagonal_Shape_double, etc...
 * @param domain refers to the [[it.unich.sci.jandom.domains.PPLDomain]] object which is the proxy for
 * the interesting methods in PPLNativeProperty.
 * @param pplobject is the PPL property we are encapsulating.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class PPLProperty[PPLNativeProperty <: AnyRef](val domain: PPLDomain[PPLNativeProperty], val pplobject: PPLNativeProperty)
  extends NumericalProperty[PPLProperty[PPLNativeProperty]] {

  type Domain = PPLDomain[PPLNativeProperty]

  def widening(that: PPLProperty[PPLNativeProperty]): PPLProperty[PPLNativeProperty] = {
    val newpplobject = domain.copyConstructor(pplobject)
    domain.upper_bound_assign(newpplobject, that.pplobject)
    domain.widening_assign(newpplobject, pplobject)
    new PPLProperty(domain, newpplobject)
  }

  /**
   * @inheritdoc
   * Since there is no standard narrowing in the PPL library, this try to use the method
   * `CC76_narrowing_assign` when it exists, returns `this` otherwise.
   * @note @inheritdoc
   */
  def narrowing(that: PPLProperty[PPLNativeProperty]): PPLProperty[PPLNativeProperty] = {
    if (domain.supportsNarrowing) {
      val newpplobject = domain.copyConstructor(that.pplobject)
      domain.narrowing_assign(newpplobject, pplobject)
      new PPLProperty(domain, newpplobject)
    } else
      this
  }

  def union(that: PPLProperty[PPLNativeProperty]): PPLProperty[PPLNativeProperty] = {
    val newpplobject = domain.copyConstructor(pplobject)
    domain.upper_bound_assign(newpplobject, that.pplobject)
    new PPLProperty(domain, newpplobject)
  }

  def intersection(that: PPLProperty[PPLNativeProperty]): PPLProperty[PPLNativeProperty] = {
    val newpplobject = domain.copyConstructor(pplobject)
    domain.intersection_assign(newpplobject, that.pplobject)
    new PPLProperty(domain, newpplobject)
  }

  def nonDeterministicAssignment(n: Int): PPLProperty[PPLNativeProperty] = {
    val newpplobject = domain.copyConstructor(pplobject)
    domain.unconstrain_space_dimension(newpplobject, new Variable(n))
    new PPLProperty(domain, newpplobject)
  }

  def linearAssignment(n: Int, lf: LinearForm[Double]): PPLProperty[PPLNativeProperty] = {
    val (le,den)= PPLUtils.toPPLLinearExpression(lf)
    val newpplobject = domain.copyConstructor(pplobject)
    domain.affine_image(newpplobject, new Variable(n), le, den)
    new PPLProperty(domain, newpplobject)
  }

  def linearInequality(lf: LinearForm[Double]): PPLProperty[PPLNativeProperty] = {
    val (le, _) = PPLUtils.toPPLLinearExpression(lf)
    val newpplobject = domain.copyConstructor(pplobject)
    domain.refine_with_constraint(newpplobject, new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
    new PPLProperty(domain, newpplobject)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @note Not yet implemented.
   */
  def linearDisequality(lf: LinearForm[Double]): PPLProperty[PPLNativeProperty] = {
    val (le, _) = PPLUtils.toPPLLinearExpression(lf)
    val newpplobject1 = domain.copyConstructor(pplobject)
    val newpplobject2 = domain.copyConstructor(pplobject)
    domain.refine_with_constraint(newpplobject1, new Constraint(le, Relation_Symbol.LESS_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
    domain.refine_with_constraint(newpplobject2, new Constraint(le, Relation_Symbol.GREATER_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
    domain.upper_bound_assign(newpplobject1, newpplobject2)
    new PPLProperty(domain, newpplobject1)
  }

  def minimize(lf: LinearForm[Double]) = {
    val (le, den) = PPLUtils.toPPLLinearExpression(lf)
    val exact = new By_Reference[java.lang.Boolean](false)
    val val_n = new Coefficient(0)
    val val_d = new Coefficient(0)
    val result = domain.minimize(pplobject, le, val_n, val_d, exact)
    if (!result)
      Double.NegativeInfinity
    else
      (new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger()) divide new java.math.BigDecimal(den.getBigInteger())).doubleValue()
  }

  def maximize(lf: LinearForm[Double]) = {
    val (le, den) = PPLUtils.toPPLLinearExpression(lf)
    val exact = new By_Reference[java.lang.Boolean](false)
    val val_n = new Coefficient(0)
    val val_d = new Coefficient(0)
    val result = domain.maximize(pplobject, le, val_n, val_d, exact)
    if (!result)
      Double.PositiveInfinity
    else
      (new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger()) divide new java.math.BigDecimal(den.getBigInteger())).doubleValue()
  }

  def frequency(lf: LinearForm[Double]) = {
    val (le, den) = PPLUtils.toPPLLinearExpression(lf)
    val freq_n = new Coefficient(0)
    val freq_d = new Coefficient(0)
    val val_n = new Coefficient(0)
    val val_d = new Coefficient(0)
    val result = domain.frequency(pplobject, le, freq_n, freq_d, val_n, val_d)
    if (!result)
      None
    else
      Some((new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger()) divide new java.math.BigDecimal(den.getBigInteger())).doubleValue())
  }

  def addVariable = {
    val newpplobject = domain.copyConstructor(pplobject)
    domain.add_space_dimensions_and_embed(newpplobject, 1)
    new PPLProperty(domain, newpplobject)
  }

  def delVariable(n: Int) = {
    val newpplobject = domain.copyConstructor(pplobject)
    val dims = new Variables_Set
    dims.add(new Variable(n))
    domain.remove_space_dimensions(newpplobject, dims)
    new PPLProperty(domain, newpplobject)
  }

  def mapVariables(rho: Seq[Int]) = {
    val newpplobject = domain.copyConstructor(pplobject)
    domain.map_space_dimensions(newpplobject, PPLUtils.sequenceToPartialFunction(rho))
    new PPLProperty(domain, newpplobject)
  }

  def minimized_constraints = domain.minimized_constraints(pplobject)

  def dimension: Int = domain.space_dimension(pplobject).toInt

  def isEmpty  = domain.is_empty(pplobject)

  def isTop = domain.is_universe(pplobject)

  def isBottom = isEmpty

  def bottom = domain.bottom(domain.space_dimension(pplobject).toInt)

  def top = domain.top(domain.space_dimension(pplobject).toInt)

  def tryCompareTo[B >: PPLProperty[PPLNativeProperty]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: PPLProperty[_] =>
      if (pplobject.getClass != other.pplobject.getClass)
        return None
      else {
        val other_pplobject = other.pplobject.asInstanceOf[PPLNativeProperty]
        if (pplobject equals other_pplobject) Some(0)
        else if (domain.strictly_contains(pplobject, other_pplobject)) Some(1)
        else if (domain.strictly_contains(other_pplobject, pplobject)) Some(-1)
        else None
      }
    case _ => None
  }

  override def hashCode: Int = pplobject.hashCode

  def mkString(vars: Seq[String]): String = PPLUtils.constraintsToString(domain.minimized_constraints(pplobject), vars)
}
