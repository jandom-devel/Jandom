/**
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
 *
 * (c) 2012 Gianluca Amato
 */

package it.unich.sci.jandom
package domains

import utils.PPLUtils
import parma_polyhedra_library._

/** 
 * This is the universal PPL numerical property. It is able to represent (almost) any property
 * representable by PPL. It only requires that some methods and constructors are defined in the
 * PPL class, and access them using reflection. It does not currently works with polyhedra due
 * to the fact both C_Polyhedron and NCC_Polyhedron inherit from a base class Polyhedron.
 * 
 * Since it uses reflexivity, this should be slower than a direct implementation such as 
 * PPLBoxDouble, but probably the overhead of native code execution is bigger than the overhead
 * of reflextion.
 *
 * @tparam PPLNativeProperty is the PPL class implementing the abstract property, such as Double_Box,
 * Octagonal_Shape_double, etc...
 * @param domain refers to the [[it.unich.sci.jandom.domain.PPLDomain] object which is the proxy for
 * the interesting methods in PPLNativeProperty.
 * @param pplobject is the PPL property we are encapsulating. 
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class PPLProperty[PPLNativeProperty <: AnyRef](private val domain: PPLDomain[PPLNativeProperty], private val pplobject: PPLNativeProperty)
  extends NumericalProperty[PPLProperty[PPLNativeProperty]] {
  
  def widening(that: PPLProperty[PPLNativeProperty]): PPLProperty[PPLNativeProperty] = {
    val newpplobject = domain.copyConstructor(pplobject)
    domain.upper_bound_assign(newpplobject, that.pplobject)
    domain.widening_assign(newpplobject, pplobject)
    new PPLProperty(domain, newpplobject)
  }

  /**
   * Since there is no standard narrowing in the PPL library, this is a fake narrowing which
   * always return `this`.
   */
  def narrowing(that: PPLProperty[PPLNativeProperty]): PPLProperty[PPLNativeProperty] = {
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

  def linearAssignment(n: Int, coeff: Array[Double], known: Double): PPLProperty[PPLNativeProperty] = {
    val newpplobject = domain.copyConstructor(pplobject)
    domain.affine_image(newpplobject, new Variable(n), PPLUtils.toPPLLinearExpression(coeff, known), new Coefficient(1))
    new PPLProperty(domain, newpplobject)
  }

  def linearInequality(coeff: Array[Double], known: Double): PPLProperty[PPLNativeProperty] = {
    val le = PPLUtils.toPPLLinearExpression(coeff, known)
    val newpplobject = domain.copyConstructor(pplobject)
    domain.refine_with_constraint(newpplobject, new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
    new PPLProperty(domain, newpplobject)
  }

  def linearDisequality(coeff: Array[Double], known: Double): PPLProperty[PPLNativeProperty] = {
    throw new IllegalAccessException("Unimplemented feature");
  }

  def dimension: Int = domain.space_dimension(pplobject).toInt

  def isEmpty: Boolean = domain.is_empty(pplobject)

  def isFull: Boolean = domain.is_universe(pplobject)
  
  def empty() = domain.empty(domain.space_dimension(pplobject).toInt)

  def full() = domain.full(domain.space_dimension(pplobject).toInt)
 
  def tryCompareTo[B >: PPLProperty[PPLNativeProperty]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: PPLProperty[_] =>
      if (pplobject.getClass != other.pplobject.getClass)
        return None
      else {
        val other_pplobject = other.pplobject.asInstanceOf[PPLNativeProperty]
        if (pplobject == other_pplobject) Some(0)
        else if (domain.strictly_contains(pplobject, other_pplobject)) Some(1)
        else if (domain.strictly_contains(other_pplobject, pplobject)) Some(-1)
        else None
      }
    case _ => None
  }

  override def equals(other: Any): Boolean = other match {
    case other: PPLProperty[_] => pplobject.equals(other.pplobject)
    case _ => false
  }

  override def hashCode: Int = pplobject.hashCode

  def mkString(vars: IndexedSeq[String]): Seq[String] = {
    //val cs: Constraint_System = domain.minimized_constraints(pplobject)
    PPLUtils.replaceOutputWithVars(pplobject.toString, vars)
  }
}

/**
 * This is the factory for PPLDomain objects. It contains handles to the methods in the PPL object
 * we want to call, and plays the role of a proxy for the PPLProperty class.
 * @tparam PPLNativeProperty is the PPL class implementing the abstract property, such as Double_Box,
 * Octagonal_Shape_double, etc... 
 */
class PPLDomain[PPLNativeProperty <: AnyRef: Manifest] extends NumericalDomain[PPLProperty[PPLNativeProperty]] {
  PPLInitializer
  
  private val PPLClass: java.lang.Class[PPLNativeProperty] = implicitly[Manifest[PPLNativeProperty]].erasure.asInstanceOf[java.lang.Class[PPLNativeProperty]]
  private val constructorHandle = PPLClass.getConstructor(classOf[Long], classOf[Degenerate_Element])
  private val copyConstructorHandle = PPLClass.getConstructor(PPLClass)
  private val upperBoundAssignHandle = PPLClass.getMethod("upper_bound_assign", PPLClass)
  private val intersectionAssignHandle = PPLClass.getMethod("intersection_assign", PPLClass)
  private val wideningAssignHandle = PPLClass.getMethod("widening_assign", PPLClass, classOf[By_Reference[Int]])
  private val affineImageHandle = PPLClass.getMethod("affine_image", classOf[Variable], classOf[Linear_Expression], classOf[Coefficient])
  private val refineWithConstraintHandle = PPLClass.getMethod("refine_with_constraint", classOf[Constraint])
  private val spaceDimensionHandle = PPLClass.getMethod("space_dimension")
  private val strictlyContainsHandle = PPLClass.getMethod("strictly_contains", PPLClass)
  private val isEmptyHandle = PPLClass.getMethod("is_empty")
  private val isUniverseHandle = PPLClass.getMethod("is_universe")
  //private val minimizedConstraintsHandle = PPLClass.getMethod("minimized_constraints")

  private[domains] def constructor(n: Int, el: Degenerate_Element) = constructorHandle.newInstance(n: java.lang.Integer, el)
  private[domains] def copyConstructor(pplobject: PPLNativeProperty) = copyConstructorHandle.newInstance(pplobject)
  private[domains] def upper_bound_assign(me: PPLNativeProperty, that: PPLNativeProperty) = upperBoundAssignHandle.invoke(me, that)
  private[domains] def widening_assign(me: PPLNativeProperty, that: PPLNativeProperty) = wideningAssignHandle.invoke(me, that, null)
  private[domains] def intersection_assign(me: PPLNativeProperty, that: PPLNativeProperty) = wideningAssignHandle.invoke(me, that)
  private[domains] def affine_image(me: PPLNativeProperty, v: Variable, le: Linear_Expression, coeff: Coefficient) = affineImageHandle.invoke(me, v, le, coeff)
  private[domains] def refine_with_constraint(me: PPLNativeProperty, c: Constraint) = refineWithConstraintHandle.invoke(me, c)
  private[domains] def space_dimension(me: PPLNativeProperty) = spaceDimensionHandle.invoke(me).asInstanceOf[Long]
  private[domains] def strictly_contains(me: PPLNativeProperty, that: PPLNativeProperty) = strictlyContainsHandle.invoke(me, that).asInstanceOf[Boolean]
  private[domains] def is_empty(me: PPLNativeProperty) = isEmptyHandle.invoke(me).asInstanceOf[Boolean]
  private[domains] def is_universe(me: PPLNativeProperty) = isUniverseHandle.invoke(me).asInstanceOf[Boolean]
  //private[domains] def minimized_constraints(me: PPLNativeProperty) = minimizedConstraintsHandle.invoke(me).asInstanceOf[Constraint_System]

  def full(n: Int): PPLProperty[PPLNativeProperty] = {
    val pplobject = constructor(n, Degenerate_Element.UNIVERSE)
    new PPLProperty[PPLNativeProperty](this, pplobject)
  }

  def empty(n: Int): PPLProperty[PPLNativeProperty] = {
    val pplobject = constructor(n, Degenerate_Element.EMPTY)
    new PPLProperty[PPLNativeProperty](this, pplobject)
  }
}
