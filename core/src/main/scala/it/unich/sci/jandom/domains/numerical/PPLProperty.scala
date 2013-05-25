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

import parma_polyhedra_library._

/**
 * This is the universal PPL numerical property. It is able to represent (almost) any property
 * representable by PPL. It only requires that some methods and constructors are defined in the
 * PPL class, and access them using reflection. Since it uses reflexivity, this is slower than
 * a direct implementation.
 *
 * @constructor creates a new PPLProperty object
 * @tparam PPLNativeProperty is the PPL class implementing the abstract property, such as Double_Box,
 * Octagonal_Shape_double, etc...
 * @param domain refers to the [[it.unich.sci.jandom.domains.PPLDomain]] object which is the proxy for
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
   * @inheritdoc
   * Since there is no standard narrowing in the PPL library, this try to use the method
   * `CC76_narrowing_assign` when it exists, returns `this` otherwise.
   * @note @inheritdoc
   */
  def narrowing(that: PPLProperty[PPLNativeProperty]): PPLProperty[PPLNativeProperty] = {
    if (domain.supportsNarrowing) {
      val newpplobject = domain.copyConstructor(pplobject)
      domain.narrowing_assign(newpplobject, that.pplobject)
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

  /**
   * @inheritdoc
   * @note @inheritdoc
   * @note Not yet implemented.
   */
  def linearDisequality(coeff: Array[Double], known: Double): PPLProperty[PPLNativeProperty] = {
    val le = PPLUtils.toPPLLinearExpression(coeff, known)
    val newpplobject1 = domain.copyConstructor(pplobject)
    val newpplobject2 = domain.copyConstructor(pplobject)
    domain.refine_with_constraint(newpplobject1, new Constraint(le, Relation_Symbol.LESS_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
    domain.refine_with_constraint(newpplobject2, new Constraint(le, Relation_Symbol.GREATER_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
    domain.upper_bound_assign(newpplobject1, newpplobject2)
    new PPLProperty(domain, newpplobject1)
  }

  def minimize(coeff: Array[Double], known: Double) = {
    val le = PPLUtils.toPPLLinearExpression(coeff, known)
    val exact = new By_Reference[java.lang.Boolean](false)
    val val_n = new Coefficient(0)
    val val_d = new Coefficient(0)
    val result = domain.minimize(pplobject, le, val_n, val_d, exact)
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
    val result = domain.maximize(pplobject, le, val_n, val_d, exact)
    if (!result)
      Double.PositiveInfinity
    else
      (new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger())).doubleValue()
  }

  def frequency(coeff: Array[Double], known: Double) = {
    val le = PPLUtils.toPPLLinearExpression(coeff, known)
    val freq_n = new Coefficient(0)
    val freq_d = new Coefficient(0)
    val val_n = new Coefficient(0)
    val val_d = new Coefficient(0)
    val result = domain.frequency(pplobject, le, freq_n, freq_d, val_n, val_d)
    if (!result)
      None
    else
      Some((new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger())).doubleValue())
  }

  def addDimension = {
    val newpplobject = domain.copyConstructor(pplobject)
    domain.add_space_dimensions_and_embed(newpplobject, 1)
    new PPLProperty(domain, newpplobject)
  }

  def delDimension(n: Int) = {
    val newpplobject = domain.copyConstructor(pplobject)
    val dims = new Variables_Set
    dims.add(new Variable(n))
    domain.remove_space_dimensions(newpplobject, dims)
    new PPLProperty(domain, newpplobject)
  }

  def mapDimensions(rho: Seq[Int]) = {
    val newpplobject = domain.copyConstructor(pplobject)
    val pf = new Partial_Function
    for ((newi, i) <- rho.zipWithIndex; if newi >= 0) {
      pf.insert(i, newi)
    }
    domain.map_space_dimensions(newpplobject, pf)
    new PPLProperty(domain, newpplobject)
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
    import collection.JavaConversions._

    val vs = new Variable_Stringifier {
      def stringify(x: Long) = vars(x.toInt)
    }
    Variable.setStringifier(vs)
    val result = for (c <- domain.minimized_constraints(pplobject))
      yield c.toString
    Variable.setStringifier(null)
    result
  }
}

/**
 * This is the factory for PPLDomain objects. It contains handles to the methods in the PPL object
 * we want to call, and plays the role of a proxy for the PPLProperty class.
 * @tparam PPLNativeProperty is the PPL class implementing the abstract property, such as Double_Box,
 * Octagonal_Shape_double, etc...
 */
class PPLDomain[PPLNativeProperty <: AnyRef: Manifest] extends NumericalDomain {

  type Property = PPLProperty[PPLNativeProperty]

  PPLInitializer

  /*
   * The class object correspondening to PPLNativeProperty
   */

  private val myClass =
    implicitly[Manifest[PPLNativeProperty]].runtimeClass.asInstanceOf[java.lang.Class[PPLNativeProperty]]

  /*
   * The use of otherClass is a sort of hack, needed since C_Polyhedron is a subclass of Polyhedron.
   * Some methods takes a Polyhedron as a secondary parameter, and getMethod requires the precise
   * signature of methods.
   */

  private val otherClass = {
    val polyhedronClass = classOf[Polyhedron]
    if (polyhedronClass.isAssignableFrom(myClass))
      polyhedronClass
    else
      myClass
  }

  private val constructorHandle = myClass.getConstructor(classOf[Long], classOf[Degenerate_Element])
  private val copyConstructorHandle = myClass.getConstructor(myClass)
  private val upperBoundAssignHandle = myClass.getMethod("upper_bound_assign", otherClass)
  private val intersectionAssignHandle = myClass.getMethod("intersection_assign", otherClass)
  private val wideningAssignHandle = myClass.getMethod("widening_assign", otherClass, classOf[By_Reference[Int]])
  private val affineImageHandle = myClass.getMethod("affine_image", classOf[Variable], classOf[Linear_Expression], classOf[Coefficient])
  private val refineWithConstraintHandle = myClass.getMethod("refine_with_constraint", classOf[Constraint])
  private val spaceDimensionHandle = myClass.getMethod("space_dimension")
  private val strictlyContainsHandle = myClass.getMethod("strictly_contains", otherClass)
  private val isEmptyHandle = myClass.getMethod("is_empty")
  private val isUniverseHandle = myClass.getMethod("is_universe")
  private val minimizedConstraintsHandle = myClass.getMethod("minimized_constraints")
  private val unconstrainSpaceDimensionHandle = myClass.getMethod("unconstrain_space_dimension", classOf[Variable])
  private val addSpaceDimensionsAndEmbedHandle = myClass.getMethod("add_space_dimensions_and_embed", classOf[Long])
  private val removeSpaceDimensionsHandle = myClass.getMethod("remove_space_dimensions", classOf[Variables_Set])
  private val mapSpaceDimensionsHandle = myClass.getMethod("map_space_dimensions", classOf[Partial_Function])
  private val minimizeHandle = myClass.getMethod("minimize", classOf[Linear_Expression], classOf[Coefficient], classOf[Coefficient], classOf[By_Reference[java.lang.Boolean]])
  private val maximizeHandle = myClass.getMethod("maximize", classOf[Linear_Expression], classOf[Coefficient], classOf[Coefficient], classOf[By_Reference[java.lang.Boolean]])
  private val frequencyHandle = myClass.getMethod("frequency", classOf[Linear_Expression], classOf[Coefficient], classOf[Coefficient], classOf[Coefficient], classOf[Coefficient])

  private val narrowingAssignHandle = try {

    myClass.getMethod("CC76_narrowing_assign", otherClass)
  } catch {
    case _: Throwable => null
  }

  private[domains] def constructor(n: Int, el: Degenerate_Element) = constructorHandle.newInstance(n: java.lang.Integer, el)
  private[domains] def copyConstructor(pplobject: PPLNativeProperty) = copyConstructorHandle.newInstance(pplobject)
  private[domains] def upper_bound_assign(me: PPLNativeProperty, that: PPLNativeProperty) = upperBoundAssignHandle.invoke(me, that)
  private[domains] def widening_assign(me: PPLNativeProperty, that: PPLNativeProperty) = wideningAssignHandle.invoke(me, that, null)
  private[domains] def intersection_assign(me: PPLNativeProperty, that: PPLNativeProperty) = intersectionAssignHandle.invoke(me, that)
  private[domains] def affine_image(me: PPLNativeProperty, v: Variable, le: Linear_Expression, coeff: Coefficient) = affineImageHandle.invoke(me, v, le, coeff)
  private[domains] def refine_with_constraint(me: PPLNativeProperty, c: Constraint) = refineWithConstraintHandle.invoke(me, c)
  private[domains] def space_dimension(me: PPLNativeProperty) = spaceDimensionHandle.invoke(me).asInstanceOf[Long]
  private[domains] def strictly_contains(me: PPLNativeProperty, that: PPLNativeProperty) = strictlyContainsHandle.invoke(me, that).asInstanceOf[Boolean]
  private[domains] def is_empty(me: PPLNativeProperty) = isEmptyHandle.invoke(me).asInstanceOf[Boolean]
  private[domains] def is_universe(me: PPLNativeProperty) = isUniverseHandle.invoke(me).asInstanceOf[Boolean]
  private[domains] def minimized_constraints(me: PPLNativeProperty) = minimizedConstraintsHandle.invoke(me).asInstanceOf[Constraint_System]
  private[domains] def unconstrain_space_dimension(me: PPLNativeProperty, v: Variable) = unconstrainSpaceDimensionHandle.invoke(me, v)
  private[domains] def add_space_dimensions_and_embed(me: PPLNativeProperty, l: Long) = addSpaceDimensionsAndEmbedHandle.invoke(me, l: java.lang.Long)
  private[domains] def remove_space_dimensions(me: PPLNativeProperty, vars: Variables_Set) = removeSpaceDimensionsHandle.invoke(me, vars)
  private[domains] def narrowing_assign(me: PPLNativeProperty, that: PPLNativeProperty) = narrowingAssignHandle.invoke(me, that)
  private[domains] def map_space_dimensions(me: PPLNativeProperty, pf: Partial_Function) = mapSpaceDimensionsHandle.invoke(me, pf)
  private[domains] def minimize(me: PPLNativeProperty, le: Linear_Expression, val_n: Coefficient, val_d: Coefficient, exact: By_Reference[java.lang.Boolean]) =
    minimizeHandle.invoke(me, le, val_n, val_d, exact).asInstanceOf[java.lang.Boolean].booleanValue()
  private[domains] def maximize(me: PPLNativeProperty, le: Linear_Expression, val_n: Coefficient, val_d: Coefficient, exact: By_Reference[java.lang.Boolean]) =
    maximizeHandle.invoke(me, le, val_n, val_d, exact).asInstanceOf[java.lang.Boolean].booleanValue()
  private[domains] def frequency(me: PPLNativeProperty, le: Linear_Expression, freq_n: Coefficient, freq_d: Coefficient, val_n: Coefficient, val_d: Coefficient) =
    frequencyHandle.invoke(me, le, freq_n, freq_d, val_n, val_d).asInstanceOf[java.lang.Boolean].booleanValue()

  /**
   * It is true if `PPLNativeProperty` has the `CC76_narrowing_assign` method.
   */
  val supportsNarrowing = narrowingAssignHandle != null

  def full(n: Int): PPLProperty[PPLNativeProperty] = {
    val pplobject = constructor(n, Degenerate_Element.UNIVERSE)
    new PPLProperty[PPLNativeProperty](this, pplobject)
  }

  def empty(n: Int): PPLProperty[PPLNativeProperty] = {
    val pplobject = constructor(n, Degenerate_Element.EMPTY)
    new PPLProperty[PPLNativeProperty](this, pplobject)
  }
}
