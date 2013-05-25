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

package it.unich.sci.jandom.domains.numerical

import it.unich.sci.jandom.utils.PPLUtils
import parma_polyhedra_library.C_Polyhedron
import parma_polyhedra_library.Coefficient
import parma_polyhedra_library.Constraint
import parma_polyhedra_library.Degenerate_Element
import parma_polyhedra_library.Linear_Expression_Coefficient
import parma_polyhedra_library.Partial_Function
import parma_polyhedra_library.Relation_Symbol
import parma_polyhedra_library.Variable
import parma_polyhedra_library.Variables_Set
import parma_polyhedra_library.By_Reference

/**
 * The domain for not necessarily closed polyhedra implemented within $PPL. This is essentially
 * a wrapper transforming methods of `C_Polyhedron` to methods of `NumericalProperty`. We clone
 * objects in order have an immutable class.
 * @param pplpolyhedron an object of class `C_Polyhedron` which is the $PPL wrapped object.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class PPLCPolyhedron(private val pplpolyhedron: C_Polyhedron) extends NumericalProperty[PPLCPolyhedron] {

  override def widening(that: PPLCPolyhedron): PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.upper_bound_assign(that.pplpolyhedron)
    newpplpolyhedron.widening_assign(pplpolyhedron, null)
    new PPLCPolyhedron(newpplpolyhedron)
  }

  /**
   * @inheritdoc
   * Since there is no narrowing for polyehdra in the PPL library, this is a fake narrowing which
   * always return `this`.
   * @note @inheritdoc
   */
  def narrowing(that: PPLCPolyhedron): PPLCPolyhedron = {
    this
  }

  def union(that: PPLCPolyhedron): PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.upper_bound_assign(that.pplpolyhedron)
    new PPLCPolyhedron(newpplpolyhedron)
  }

  def intersection(that: PPLCPolyhedron): PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.intersection_assign(that.pplpolyhedron)
    new PPLCPolyhedron(newpplpolyhedron)
  }

  def nonDeterministicAssignment(n: Int): PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.unconstrain_space_dimension(new Variable(n))
    new PPLCPolyhedron(newpplpolyhedron)
  }

  def linearAssignment(n: Int, coeff: Array[Double], known: Double): PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.affine_image(new Variable(n), PPLUtils.toPPLLinearExpression(coeff, known), new Coefficient(1))
    new PPLCPolyhedron(newpplpolyhedron)
  }

  def linearInequality(coeff: Array[Double], known: Double): PPLCPolyhedron = {
    val le = PPLUtils.toPPLLinearExpression(coeff, known)
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
    new PPLCPolyhedron(newpplpolyhedron)
  }

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @note The `C_Polyhedron` class in the PPL treat strict relational operators as non-strict ones. Hence,
   * this method simply returns `this`.
   */
  def linearDisequality(coeff: Array[Double], known: Double): PPLCPolyhedron = {
    this
  }

  def minimize(coeff: Array[Double], known: Double) = {
    val le = PPLUtils.toPPLLinearExpression(coeff, known)
    val exact = new By_Reference[java.lang.Boolean](false)
    val val_n = new Coefficient(0)
    val val_d = new Coefficient(0)
    val result = pplpolyhedron.minimize(le, val_n, val_d, exact)
    if (!result)
      Double.NegativeInfinity
    else
      (new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger())).doubleValue()
  }

  def maximize(coeff: Array[Double], known: Double) = {
    val le = PPLUtils.toPPLLinearExpression(coeff, known)
    val exact = new By_Reference[java.lang.Boolean](false)
    val val_n = new Coefficient(0)
    val val_d = new Coefficient(0)
    val result = pplpolyhedron.maximize(le, val_n, val_d, exact)
    if (!result)
      Double.NegativeInfinity
    else
      (new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger())).doubleValue()
  }

  def frequency(coeff: Array[Double], known: Double) = {
    val le = PPLUtils.toPPLLinearExpression(coeff, known)
    val freq_n = new Coefficient(0)
    val freq_d = new Coefficient(0)
    val val_n = new Coefficient(0)
    val val_d = new Coefficient(0)
    val result = pplpolyhedron.frequency(le, freq_n, freq_d, val_n, val_d)
    if (!result)
      None
    else
      Some((new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger())).doubleValue())
  }

  def addDimension: PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.add_space_dimensions_and_embed(1)
    new PPLCPolyhedron(newpplpolyhedron)
  }

  def delDimension(n: Int): PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    val dims = new Variables_Set
    dims.add(new Variable(n))
    newpplpolyhedron.remove_space_dimensions(dims)
    new PPLCPolyhedron(newpplpolyhedron)
  }

  def mapDimensions(rho: Seq[Int]) = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    val pf = new Partial_Function
    for ((newi, i) <- rho.zipWithIndex; if newi >= 0) {
      pf.insert(i, newi)
    }
    newpplpolyhedron.map_space_dimensions(pf)
    new PPLCPolyhedron(newpplpolyhedron)
  }

  def dimension(): Int = pplpolyhedron.space_dimension.toInt

  def isEmpty(): Boolean = pplpolyhedron.is_empty

  def isFull(): Boolean = pplpolyhedron.is_universe

  def empty = PPLCPolyhedron.empty(pplpolyhedron.space_dimension.toInt)

  def full = PPLCPolyhedron.full(pplpolyhedron.space_dimension.toInt)

  def tryCompareTo[B >: PPLCPolyhedron](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: PPLCPolyhedron =>
      if (pplpolyhedron == other.pplpolyhedron) Some(0) else if (pplpolyhedron strictly_contains other.pplpolyhedron) Some(1) else if (other.pplpolyhedron strictly_contains pplpolyhedron) Some(-1)
      else None
    case _ => None
  }

  override def equals(other: Any): Boolean = other match {
    case other: PPLCPolyhedron => pplpolyhedron.equals(other.pplpolyhedron)
    case _ => false
  }

  override def hashCode: Int = pplpolyhedron.hashCode

  def mkString(vars: IndexedSeq[String]): Seq[String] =
    PPLUtils.replaceOutputWithVars(pplpolyhedron.toString, vars)
}

/**
 * This is the factory for ``PPLBoxDouble`` properties.
 */
object PPLCPolyhedron extends NumericalDomain {
  PPLInitializer

  type Property = PPLCPolyhedron

  def full(n: Int): PPLCPolyhedron = {
    val pplpolyhedron = new C_Polyhedron(n, Degenerate_Element.UNIVERSE)
    new PPLCPolyhedron(pplpolyhedron)
  }

  def empty(n: Int): PPLCPolyhedron = {
    val pplpolyhedron = new C_Polyhedron(n, Degenerate_Element.EMPTY)
    new PPLCPolyhedron(pplpolyhedron)
  }
}
