/**
 * Copyright 2013, 2016, 2017 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.domains.{DomainTransformation, WideningDescription}
import it.unich.scalafix.Box
import parma_polyhedra_library._

/**
 * This is the domain of PPL properties.  It is able to represent (almost) any property
 * representable by PPL. It only requires that some methods and constructors are defined in the
 * PPL class, and access them using reflection. Therefore, this is slower than a direct implementation
 *
 * This class contains handles to the methods in the PPL object we want to call, and plays the role of
 * a proxy for the PPLProperty class.
 * @tparam PPLNativeProperty is the PPL class implementing the abstract property, such as Double_Box,
 * Octagonal_Shape_double, etc...
 */
class PPLDomain[PPLNativeProperty <: AnyRef: Manifest] extends NumericalDomain {

  type Property = PPLProperty[PPLNativeProperty]

  PPLInitializer

  /*
   * The class object corresponding to PPLNativeProperty
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
  private val minimizedCongruencesHandle = myClass.getMethod("minimized_congruences")
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
  private[domains] def minimized_congruences(me: PPLNativeProperty) = minimizedCongruencesHandle.invoke(me).asInstanceOf[Congruence_System]
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

  private val wideningsList = for {
    m <- myClass.getMethods()
    name = m.getName
    if name.toString.endsWith("_widening_assign")
    wideningName = name.toString.stripSuffix("_widening_assign")
  } yield
      WideningDescription(wideningName, s"The PPL widening using the ${name.toString} method",
         Box.apply[Property] { (a: Property, b: Property) => {
           val newpplobject = copyConstructor(a.pplobject)
           upper_bound_assign(newpplobject,b.pplobject)
           m.invoke(newpplobject, a.pplobject, null)
           new PPLProperty(this, newpplobject)
         }
      })

  val widenings = WideningDescription.default[Property] +: wideningsList.toSeq

  def top(n: Int): Property = {
    val pplobject = constructor(n, Degenerate_Element.UNIVERSE)
    new PPLProperty(this, pplobject)
  }

  def bottom(n: Int): Property = {
    val pplobject = constructor(n, Degenerate_Element.EMPTY)
    new PPLProperty(this, pplobject)
  }

  /**
   * Build a PPL property from a PPL property of other type. Conversion is slow because the right constructor
   * is looked up at runtime.
   * @param x the source `PPLProperty`
   * @return x transformed into a `PPLPropert[PPLNativeProperty]`
   */
  def apply(x: PPLProperty[_ <: AnyRef]): Property = {
    val srcClass = x.domain.myClass
    val constructor = myClass.getConstructor(srcClass, classOf[Complexity_Class])
    val pplobject = constructor.newInstance(x.pplobject, Complexity_Class.SIMPLEX_COMPLEXITY)
    new PPLProperty(this, pplobject)
  }
}

object PPLDomain {
  // TODO: evaluate whether we should mix with CachedTopBottom
  def apply[PPLNativeProperty <: AnyRef: Manifest]() = new PPLDomain[PPLNativeProperty]

  implicit object PPLtoPPL extends DomainTransformation[PPLDomain[_ <: AnyRef], PPLDomain[_ <: AnyRef]] {
    private def getTransformer[S <: AnyRef, D <: AnyRef](src: PPLDomain[S], dst: PPLDomain[D]): src.Property => dst.Property = {
      val constructor = dst.myClass.getConstructor(src.myClass, classOf[Complexity_Class])
      val transformer = { (x: src.Property) =>
        val pplobject = constructor.newInstance(x.pplobject, Complexity_Class.SIMPLEX_COMPLEXITY)
        new PPLProperty(dst, pplobject)
      }
      transformer
    }
    def apply(src: PPLDomain[_ <: AnyRef], dst: PPLDomain[_ <: AnyRef]): src.Property => dst.Property = getTransformer(src, dst)
  }
}
