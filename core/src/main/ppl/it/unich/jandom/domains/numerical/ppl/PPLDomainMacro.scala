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

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import it.unich.jandom.domains.DomainTransformation
import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.domains.numerical.NumericalProperty
import parma_polyhedra_library.Polyhedron
import it.unich.jandom.widenings.Widening
import it.unich.scalafix.Box

/**
 * This is the ancestor of all PPL-based macro-generated domains.
 * @tparam PPLType the PPL class of the numerical properties handled by this domain.
 */
abstract class PPLDomainMacro[PPLType] extends NumericalDomain {
  type Property <: PPLPropertyMacro[Property, PPLType]

  /**
   * Given an object `x` of type `PPLType`, wraps it into an abstract property.
   */
  def apply(x: PPLType): Property
}

/**
 * This is the ancestor of all PPL-based macro-generated abstract properties. It is only a marker class.
 * @tparam Property the concrete instance of a `PPLPropertyMacro` for F-bounded polymorphism.
 * @tparam PPLType the PPL class of the numerical properties handled by this domain.
 */
abstract class PPLPropertyMacro[Property <: PPLPropertyMacro[Property, PPLType], PPLType] extends NumericalProperty[Property] {
  this: Property =>
  type Domain <: PPLDomainMacro[PPLType]

  /**
   * The underlying PPL object corresponding to this abstract property.
   */
  val pplobject: PPLType
}

/**
 * This class contains macros for compile-time creation of PPL backed numerical properties and domain.
 * The aim is similar to the class [[it.unich.jandom.domains.numerical.ppl.PPLDomain]], but while
 * that uses reflection, here we use macros to generate much faster code.
 */
object PPLDomainMacro {

  /**
   * This method returns a NumericalDomain for the PPL class specified as type parameter.
   * @tparam PPLType the PPL class of the numerical properties handled by this domain
   *
   */
  def apply[PPLType]: PPLDomainMacro[PPLType] = macro PPLDomainImpl[PPLType]

  /**
   * This method returns a DomainTransformation between PPL macro-based domains
   */
  def transformer[PPLSource, PPLDest]: DomainTransformation[PPLDomainMacro[PPLSource], PPLDomainMacro[PPLDest]] = macro PPLTransformationImpl[PPLSource, PPLDest]

  /**
   * This is the implementation of the transformer method.
   */
  def PPLTransformationImpl[PPLSource, PPLDest](c: Context)(implicit PPLSourceTypeTag: c.WeakTypeTag[PPLSource], PPLDestTypeTag: c.WeakTypeTag[PPLDest]) = {
    import c.universe._

    val outputTree = q"""
      import it.unich.jandom.domains.DomainTransformation
      import parma_polyhedra_library._

      object PPLtoPPL extends DomainTransformation[PPLDomainMacro[$PPLSourceTypeTag], PPLDomainMacro[$PPLDestTypeTag]] {
        def apply(src: PPLDomainMacro[$PPLSourceTypeTag], dst: PPLDomainMacro[$PPLDestTypeTag]): src.Property => dst.Property = { (x) =>
          dst(new $PPLDestTypeTag(x.pplobject))
        }
      }

      PPLtoPPL
    """

    c.Expr[DomainTransformation[PPLDomainMacro[PPLSource], PPLDomainMacro[PPLDest]]](outputTree)
  }

  /**
   * This is the implementation of the `apply` method.
   * @tparam PPLType the PPL class of the numerical properties handled by this domain
   */
  def PPLDomainImpl[PPLType](c: Context)(implicit PPLTypeTag: c.WeakTypeTag[PPLType]): c.Expr[PPLDomainMacro[PPLType]] = {
    import c.universe._

    val supportsCC76Narrowing = PPLTypeTag.tpe.member(TermName("CC76_narrowing_assign")) != NoSymbol

    val widenings = for {
      m <- PPLTypeTag.tpe.members
      name = m.name
      if name.toString.endsWith("_widening_assign")
      wideningName = name.toString.stripSuffix("_widening_assign")
    } yield q"""
      WideningDescription($wideningName, "The PPL widening using the "+${name.toString}+" method",
         Box.apply[Property] { (a: Property, b: Property) => {
           val newpplobject = new $PPLTypeTag(a.pplobject)
           newpplobject.upper_bound_assign(b.pplobject)
           newpplobject.$m(a.pplobject, null)
           new Property(newpplobject)
         }
      })
   """

    val narrowing = if (supportsCC76Narrowing)
      q"""
          def narrowing(that: ThisProperty): ThisProperty = {
            val newpplobject = new $PPLTypeTag(pplobject)
            newpplobject.CC76_narrowing_assign(pplobject)
            new ThisProperty(newpplobject)
          }
       """
    else
      q"""
          def narrowing(that: ThisProperty): ThisProperty = {
            this
          }
      """

    val outputTree = q"""
      import it.unich.jandom.domains.WideningDescription
      import it.unich.jandom.domains.numerical.LinearForm
      import it.unich.jandom.domains.numerical.ppl._
      import it.unich.jandom.utils.numberext.RationalExt
      import it.unich.scalafix.Box

      import spire.math.Rational
      import parma_polyhedra_library._

      class ThisProperty(val pplobject: $PPLTypeTag) extends PPLPropertyMacro[ThisProperty, $PPLTypeTag] {
        type Domain = ThisDomain.type

        def domain = ThisDomain

        def widening(that: ThisProperty): ThisProperty = {
          val newpplobject = new $PPLTypeTag(pplobject)
          newpplobject.upper_bound_assign(that.pplobject)
          newpplobject.widening_assign(pplobject, null)
          new ThisProperty(newpplobject)
        }

       $narrowing

        def union(that: ThisProperty): ThisProperty = {
          val newpplobject = new $PPLTypeTag(pplobject)
          val x = new $PPLTypeTag(pplobject.space_dimension(), Degenerate_Element.EMPTY)
          newpplobject.upper_bound_assign(that.pplobject)
          new ThisProperty(newpplobject)
        }

        def intersection(that: ThisProperty): ThisProperty = {
          val newpplobject = new $PPLTypeTag(pplobject)
          newpplobject.intersection_assign(that.pplobject)
          new ThisProperty(newpplobject)
        }

        def nonDeterministicAssignment(n: Int): ThisProperty = {
          val newpplobject = new $PPLTypeTag(pplobject)
          newpplobject.unconstrain_space_dimension(new Variable(n))
          new ThisProperty(newpplobject)
        }

        def linearAssignment(n: Int, lf: LinearForm): ThisProperty = {
          val (le, den) = PPLUtils.toPPLLinearExpression(lf)
          val newpplobject = new $PPLTypeTag(pplobject)
          newpplobject.affine_image(new Variable(n), le, den)
          new ThisProperty(newpplobject)
        }

        def linearInequality(lf: LinearForm): ThisProperty = {
          val (le, den) = PPLUtils.toPPLLinearExpression(lf)
          val newpplobject = new $PPLTypeTag(pplobject)
          newpplobject.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
          new ThisProperty(newpplobject)
        }

        def linearDisequality(lf: LinearForm): ThisProperty = {
          val (le, den) = PPLUtils.toPPLLinearExpression(lf)
          val newpplobject1 = new $PPLTypeTag(pplobject)
          val newpplobject2 = new $PPLTypeTag(pplobject)
          newpplobject1.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
          newpplobject2.refine_with_constraint(new Constraint(le, Relation_Symbol.GREATER_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
          newpplobject1.upper_bound_assign(newpplobject2)
          new ThisProperty(newpplobject1)
        }

        def minimize(lf: LinearForm) = {
          val (le, den) = PPLUtils.toPPLLinearExpression(lf)
          val exact = new By_Reference[java.lang.Boolean](false)
          val val_n = new Coefficient(0)
          val val_d = new Coefficient(0)
          val result = pplobject.minimize(le, val_n, val_d, exact)
          if (!result)
            RationalExt.NegativeInfinity
          else
            RationalExt(val_n.getBigInteger(), val_d.getBigInteger().multiply(den.getBigInteger()))
        }

        def maximize(lf: LinearForm) = {
          val (le, den) = PPLUtils.toPPLLinearExpression(lf)
          val exact = new By_Reference[java.lang.Boolean](false)
          val val_n = new Coefficient(0)
          val val_d = new Coefficient(0)
          val result = pplobject.maximize(le, val_n, val_d, exact)
          if (!result)
            RationalExt.PositiveInfinity
          else
            RationalExt(val_n.getBigInteger(), val_d.getBigInteger().multiply(den.getBigInteger()))
        }

        def frequency(lf: LinearForm) = {
          val (le, den) = PPLUtils.toPPLLinearExpression(lf)
          val freq_n = new Coefficient(0)
          val freq_d = new Coefficient(0)
          val val_n = new Coefficient(0)
          val val_d = new Coefficient(0)
          val result = pplobject.frequency(le, freq_n, freq_d, val_n, val_d)
          if (!result)
            Option.empty
          else
            Option(Rational(val_n.getBigInteger(), val_d.getBigInteger().multiply(den.getBigInteger())))
        }

        def constraints = {
          import collection.JavaConversions._

          val cs = pplobject.minimized_constraints()
          cs flatMap PPLUtils.fromPPLConstraint
        }

        def isPolyhedral = {
          import collection.JavaConversions._
          val cs = pplobject.minimized_constraints()
          (cs forall PPLUtils.isRepresentableAsLinearForms) && pplobject.minimized_congruences().isEmpty()
        }

        def addVariable: ThisProperty = {
          val newpplobject = new $PPLTypeTag(pplobject)
          newpplobject.add_space_dimensions_and_embed(1)
          new ThisProperty(newpplobject)
        }

        def delVariable(n: Int): ThisProperty = {
          val newpplobject = new $PPLTypeTag(pplobject)
          val dims = new Variables_Set
          dims.add(new Variable(n))
          newpplobject.remove_space_dimensions(dims)
          new ThisProperty(newpplobject)
        }

        def mapVariables(rho: Seq[Int]): ThisProperty = {
          val newpplobject = new $PPLTypeTag(pplobject)
          newpplobject.map_space_dimensions(PPLUtils.sequenceToPartialFunction(rho))
          new ThisProperty(newpplobject)
        }

        def dimension: Int = pplobject.space_dimension.toInt

        def isEmpty: Boolean = pplobject.is_empty

        def isTop: Boolean = pplobject.is_universe

        def isBottom = isEmpty

        def bottom = domain.bottom(pplobject.space_dimension.toInt)

        def top = domain.top(pplobject.space_dimension.toInt)

        def tryCompareTo[B >: ThisProperty](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
          case other: ThisProperty =>
            if (pplobject == other.pplobject)
              Option(0)
            else if (pplobject strictly_contains other.pplobject)
              Option(1)
            else if (other.pplobject strictly_contains pplobject)
              Option(-1)
            else
              Option.empty
          case _ => Option.empty
        }

        override def hashCode: Int = pplobject.hashCode

        def mkString(vars: Seq[String]): String = PPLUtils.constraintsToString(pplobject.minimized_constraints(), vars)
      }

      /*
       * This is the domain of macro based PPL objects.
       */
      object ThisDomain extends PPLDomainMacro[$PPLTypeTag] {
        PPLInitializer

        type Property = ThisProperty

        def top(n: Int): ThisProperty = {
          val pplobject = new $PPLTypeTag(n, Degenerate_Element.UNIVERSE)
          new ThisProperty(pplobject)
        }

        def bottom(n: Int): ThisProperty = {
          val pplobject = new $PPLTypeTag(n, Degenerate_Element.EMPTY)
          new ThisProperty(pplobject)
        }

        def apply(x: $PPLTypeTag): ThisProperty = new ThisProperty(x)

        val widenings = Seq(..$widenings)
      }

      ThisDomain
    """

    c.Expr[PPLDomainMacro[PPLType]](outputTree)
  }
}
