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

import scala.reflect.macros.Context
import scala.language.experimental.macros

import it.unich.sci.jandom.domains.DomainTransformation
import it.unich.sci.jandom.domains.numerical.NumericalDomain
import it.unich.sci.jandom.domains.numerical.NumericalProperty

import parma_polyhedra_library._

/**
 * This is the ancestor of all PPL-based macro-generated domains. It is only a marker class.
 * @tparam PPLType the PPL class of the numerical properties handled by this domain
 */
abstract class PPLDomainMacro[PPLType] extends NumericalDomain {
  type Property <: PPLPropertyMacro[Property, PPLType]

  def apply(x: PPLType): Property
}

abstract class PPLPropertyMacro[Property <: PPLPropertyMacro[Property, PPLType], PPLType] extends NumericalProperty[Property] {
  this: Property =>
  type Domain <: PPLDomainMacro[PPLType]

  val pplobject: PPLType
}

/**
 * This class contains macros for compile-time creation of PPL backed numerical properties.
 * The aim is similar to the class [[it.unich.sci.jandom.domains.ThisProperty]], but while
 * that uses reflection, here we use macros to generate much faster code.
 */
object PPLDomainMacro {

  /**
   * This method returns a NumericalDomain for the PPL class specified as
   * type parameter.
   * @tparam PPLType the PPL class of the numerical properties handled by this domain
   *
   */
  def apply[PPLType]: PPLDomainMacro[PPLType] = macro PPLDomainImpl[PPLType]

  /**
   * This method returns a DomainTransformation between PPL macro-based domains
   */
  def transformer[PPLSource, PPLDest]: DomainTransformation[PPLDomainMacro[PPLSource], PPLDomainMacro[PPLDest]] = macro PPLTransformationImpl[PPLSource, PPLDest]

  /**
   * This is the implementaton of the transformer `method`
   */
  def PPLTransformationImpl[PPLSource: c.WeakTypeTag, PPLDest: c.WeakTypeTag](c: Context): c.Expr[DomainTransformation[PPLDomainMacro[PPLSource], PPLDomainMacro[PPLDest]]] = {
    import c.universe._
    import parma_polyhedra_library.Double_Box
    import parma_polyhedra_library.C_Polyhedron

    val template = reify {
      object PPLtoPPL extends DomainTransformation[PPLDomainMacro[Double_Box], PPLDomainMacro[C_Polyhedron]] {
        def apply(src: PPLDomainMacro[Double_Box], dst: PPLDomainMacro[C_Polyhedron]): src.Property => dst.Property = { (x) =>
          dst(new C_Polyhedron(x.pplobject))
        }
      }
    }

    val PPLSourceTypeSymbol = implicitly[c.WeakTypeTag[PPLSource]].tpe.typeSymbol
    val PPLDestTypeSymbol = implicitly[c.WeakTypeTag[PPLDest]].tpe.typeSymbol

    // Here we substitute the placeholders Double_Box and C_Polyhedron with the real types
    val templateWithSubstitution = template.tree.substituteSymbols(
      List(typeOf[Double_Box].typeSymbol, typeOf[C_Polyhedron].typeSymbol),
      List(PPLSourceTypeSymbol, PPLDestTypeSymbol))

    // Here we add the resulting domain as output of the tree
    val outputTree = templateWithSubstitution match {
      case Block(stats, expr) => Block(stats, Ident(newTermName("PPLtoPPL")))
    }

    c.Expr[DomainTransformation[PPLDomainMacro[PPLSource], PPLDomainMacro[PPLDest]]](outputTree)
  }

  /**
   * This is the implementation of the `apply` method.
   * @tparam PPLType the PPL class of the numerical properties handled by this domain
   */
  def PPLDomainImpl[PPLType: c.WeakTypeTag](c: Context): c.Expr[PPLDomainMacro[PPLType]] = {
    import c.universe._
    import parma_polyhedra_library.Double_Box
    import it.unich.sci.jandom.domains.numerical.LinearForm

    val classes = reify {
      import parma_polyhedra_library._

      /**
       * This is the generic class for PPL properties. The class actually implements boxes over doubles,
       * but all references to `Double_Box` is changed by the macro and replaced by `PPLType`.
       * @author Gianluca Amato <gamato@unich.it>
       */
      class ThisProperty(val pplobject: Double_Box) extends PPLPropertyMacro[ThisProperty, Double_Box] {
        type Domain = ThisDomain.type

        def domain = ThisDomain

        def widening(that: ThisProperty): ThisProperty = {
          val newpplobject = new Double_Box(pplobject)
          newpplobject.upper_bound_assign(that.pplobject)
          newpplobject.widening_assign(pplobject, null)
          new ThisProperty(newpplobject)
        }

        def narrowing(that: ThisProperty): ThisProperty = {
          this
        }

        def union(that: ThisProperty): ThisProperty = {
          val newpplobject = new Double_Box(pplobject)
          val x = new Double_Box(pplobject.space_dimension(), Degenerate_Element.EMPTY)
          newpplobject.upper_bound_assign(that.pplobject)
          new ThisProperty(newpplobject)
        }

        def intersection(that: ThisProperty): ThisProperty = {
          val newpplobject = new Double_Box(pplobject)
          newpplobject.intersection_assign(that.pplobject)
          new ThisProperty(newpplobject)
        }

        def nonDeterministicAssignment(n: Int): ThisProperty = {
          val newpplobject = new Double_Box(pplobject)
          newpplobject.unconstrain_space_dimension(new Variable(n))
          new ThisProperty(newpplobject)
        }

        def linearAssignment(n: Int, lf: LinearForm[Double]): ThisProperty = {
          val (le,den) = PPLUtils.toPPLLinearExpression(lf)
          val newpplobject = new Double_Box(pplobject)
          newpplobject.affine_image(new Variable(n), le, den)
          new ThisProperty(newpplobject)
        }

        def linearInequality(lf: LinearForm[Double]): ThisProperty = {
          val (le, den) = PPLUtils.toPPLLinearExpression(lf)
          val newpplobject = new Double_Box(pplobject)
          newpplobject.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
          new ThisProperty(newpplobject)
        }

        def linearDisequality(lf: LinearForm[Double]): ThisProperty = {
          val (le, den) = PPLUtils.toPPLLinearExpression(lf)
          val newpplobject1 = new Double_Box(pplobject)
          val newpplobject2 = new Double_Box(pplobject)
          newpplobject1.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
          newpplobject2.refine_with_constraint(new Constraint(le, Relation_Symbol.GREATER_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
          newpplobject1.upper_bound_assign(newpplobject2)
          new ThisProperty(newpplobject1)
        }

        def minimize(lf: LinearForm[Double]) = {
          val (le, den) = PPLUtils.toPPLLinearExpression(lf)
          val exact = new By_Reference[java.lang.Boolean](false)
          val val_n = new Coefficient(0)
          val val_d = new Coefficient(0)
          val result = pplobject.minimize(le, val_n, val_d, exact)
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
          val result = pplobject.maximize(le, val_n, val_d, exact)
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
          val result = pplobject.frequency(le, freq_n, freq_d, val_n, val_d)
          if (!result)
            None
          else
            Some((new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger()) divide new java.math.BigDecimal(den.getBigInteger())).doubleValue())
        }

        def addVariable: ThisProperty = {
          val newpplobject = new Double_Box(pplobject)
          newpplobject.add_space_dimensions_and_embed(1)
          new ThisProperty(newpplobject)
        }

        def delVariable(n: Int): ThisProperty = {
          val newpplobject = new Double_Box(pplobject)
          val dims = new Variables_Set
          dims.add(new Variable(n))
          newpplobject.remove_space_dimensions(dims)
          new ThisProperty(newpplobject)
        }

        def mapVariables(rho: Seq[Int]): ThisProperty = {
          val newpplobject = new Double_Box(pplobject)
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
            if (pplobject == other.pplobject) Some(0)
            else if (pplobject strictly_contains other.pplobject) Some(1)
            else if (other.pplobject strictly_contains pplobject) Some(-1)
            else None
          case _ => None
        }

        override def hashCode: Int = pplobject.hashCode

        def mkString(vars: Seq[String]): String = PPLUtils.constraintsToString(pplobject.minimized_constraints(), vars)
      }

      /**
       * This is the domain of macro based PPL objects.
       * @author Ganluca Amato <gamato@unich.it>
       */
      object ThisDomain extends PPLDomainMacro[PPLType] {
        PPLInitializer

        type Property = ThisProperty

        def top(n: Int): ThisProperty = {
          val pplobject = new Double_Box(n, Degenerate_Element.UNIVERSE)
          new ThisProperty(pplobject)
        }

        def bottom(n: Int): ThisProperty = {
          val pplobject = new Double_Box(n, Degenerate_Element.EMPTY)
          new ThisProperty(pplobject)
        }

        def apply(x: Double_Box): ThisProperty = new ThisProperty(x)
      }
    }

    val PPLTypeSymbol = implicitly[c.WeakTypeTag[PPLType]].tpe.typeSymbol

    // Here we substitute the placeholder Double_Box symbol with the real type
    val classesWithSubstitution = classes.tree.substituteSymbols(
      List(typeOf[Double_Box].typeSymbol),
      List(PPLTypeSymbol))

    // Here we add the resulting domain as output of the tree
    val outputTree = classesWithSubstitution match {
      case Block(stats, expr) => Block(stats, Ident(newTermName("ThisDomain")))
    }

    c.Expr[PPLDomainMacro[PPLType]](outputTree)
  }
}
