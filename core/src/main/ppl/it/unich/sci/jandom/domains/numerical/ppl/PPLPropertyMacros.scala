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

import it.unich.sci.jandom.domains.numerical.NumericalDomain
import it.unich.sci.jandom.domains.numerical.NumericalProperty

import parma_polyhedra_library._
/**
 * This class containts macros for compile-time creation of PPL backed numerical properties.
 * The aim si similar to the clas [[it.unich.sci.jandom.domains.PPLProperty]], but while
 * that uses reflection, here we use macros to generate much faster code.
 */
object PPLPropertyMacros {

  /**
   * This method returns a NumericalDomain for the PPL class specified as
   * type parameter.
   */
  def apply[PPLType]: NumericalDomain = macro PPLDomainImpl[PPLType]

  /**
   * This is the implementation of the `apply` method.
   */
  def PPLDomainImpl[PPLType: c.WeakTypeTag](c: Context): c.Expr[NumericalDomain] = {
    import c.universe._
    import parma_polyhedra_library.Double_Box
    import it.unich.sci.jandom.domains.numerical.LinearForm

    val classes = reify {
      import parma_polyhedra_library._

      /**
       * The domain for possibly opened box over doubles implemented within $PPL. This is essentially
       * a wrapper transforming methods of `Double_Box` to methods of `NumericalProperty`. We clone
       * objects in order have an immutable class.
       * @param pplbox an object of class `Double_Box` which is the $PPL wrapped object.
       * @author Gianluca Amato <amato@sci.unich.it>
       */
      class PPLProperty(private val pplbox: Double_Box) extends NumericalProperty[PPLProperty] {
    	type Domain = PPLProperty.type

    	def domain = PPLProperty

        def widening(that: PPLProperty): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          newpplbox.upper_bound_assign(that.pplbox)
          newpplbox.widening_assign(pplbox, null)
          new PPLProperty(newpplbox)
        }

        def narrowing(that: PPLProperty): PPLProperty = {
          this
        }

        def union(that: PPLProperty): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          val x = new Double_Box(pplbox.space_dimension(), Degenerate_Element.EMPTY)
          newpplbox.upper_bound_assign(that.pplbox)
          new PPLProperty(newpplbox)
        }

        def intersection(that: PPLProperty): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          newpplbox.intersection_assign(that.pplbox)
          new PPLProperty(newpplbox)
        }

        def nonDeterministicAssignment(n: Int): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          newpplbox.unconstrain_space_dimension(new Variable(n))
          new PPLProperty(newpplbox)
        }

        def linearAssignment(n: Int, lf: LinearForm[Double]): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          newpplbox.affine_image(new Variable(n), PPLUtils.toPPLLinearExpression(lf), new Coefficient(1))
          new PPLProperty(newpplbox)
        }

        def linearInequality(lf: LinearForm[Double]): PPLProperty = {
          val le = PPLUtils.toPPLLinearExpression(lf)
          val newpplbox = new Double_Box(pplbox)
          newpplbox.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
          new PPLProperty(newpplbox)
        }

        def linearDisequality(lf: LinearForm[Double]): PPLProperty = {
          val le = PPLUtils.toPPLLinearExpression(lf)
          val newpplbox1 = new Double_Box(pplbox)
          val newpplbox2 = new Double_Box(pplbox)
          newpplbox1.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
          newpplbox2.refine_with_constraint(new Constraint(le, Relation_Symbol.GREATER_THAN, new Linear_Expression_Coefficient(new Coefficient(0))))
          newpplbox1.upper_bound_assign(newpplbox2)
          new PPLProperty(newpplbox1)
        }

        def minimize(lf: LinearForm[Double]) = {
          val le = PPLUtils.toPPLLinearExpression(lf)
          val exact = new By_Reference[java.lang.Boolean](false)
          val val_n = new Coefficient(0)
          val val_d = new Coefficient(0)
          val result = pplbox.minimize(le, val_n, val_d, exact)
          if (!result)
            Double.NegativeInfinity
          else
            (new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger())).doubleValue()
        }

        def maximize(lf: LinearForm[Double]) = {
          val le = PPLUtils.toPPLLinearExpression(lf)
          val exact = new By_Reference[java.lang.Boolean](false)
          val val_n = new Coefficient(0)
          val val_d = new Coefficient(0)
          val result = pplbox.maximize(le, val_n, val_d, exact)
          if (!result)
            Double.PositiveInfinity
          else
            (new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger())).doubleValue()
        }

        def frequency(lf: LinearForm[Double]) = {
          val le = PPLUtils.toPPLLinearExpression(lf)
          val freq_n = new Coefficient(0)
          val freq_d = new Coefficient(0)
          val val_n = new Coefficient(0)
          val val_d = new Coefficient(0)
          val result = pplbox.frequency(le, freq_n, freq_d, val_n, val_d)
          if (!result)
            None
          else
            Some((new java.math.BigDecimal(val_n.getBigInteger()) divide new java.math.BigDecimal(val_d.getBigInteger())).doubleValue())
        }

        def addVariable: PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          newpplbox.add_space_dimensions_and_embed(1)
          new PPLProperty(newpplbox)
        }

        def delVariable(n: Int): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          val dims = new Variables_Set
          dims.add(new Variable(n))
          newpplbox.remove_space_dimensions(dims)
          new PPLProperty(newpplbox)
        }

        def mapVariables(rho: Seq[Int]): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          newpplbox.map_space_dimensions(PPLUtils.sequenceToPartialFunction(rho))
          new PPLProperty(newpplbox)
        }

        def dimension: Int = pplbox.space_dimension.toInt

        def isEmpty: Boolean = pplbox.is_empty

        def isTop: Boolean = pplbox.is_universe

        def isBottom = isEmpty

        def bottom = PPLProperty.bottom(pplbox.space_dimension.toInt)

        def top = PPLProperty.top(pplbox.space_dimension.toInt)

        def tryCompareTo[B >: PPLProperty](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
          case other: PPLProperty =>
            if (pplbox == other.pplbox) Some(0) else if (pplbox strictly_contains other.pplbox) Some(1) else if (other.pplbox strictly_contains pplbox) Some(-1)
            else None
          case _ => None
        }

        override def equals(other: Any): Boolean = other match {
          case other: PPLProperty => pplbox.equals(other.pplbox)
          case _ => false
        }

        override def hashCode: Int = pplbox.hashCode

        def mkString(vars: Seq[String]): String = PPLUtils.constraintsToString(pplbox.minimized_constraints(), vars)
      }

      /**
       * This is the factory for ``PPLProperty`` properties.
       */
      object PPLProperty extends NumericalDomain {
        PPLInitializer

        type Property = PPLProperty

        def top(n: Int): PPLProperty = {
          val pplbox = new Double_Box(n, Degenerate_Element.UNIVERSE)
          new PPLProperty(pplbox)
        }

        def bottom(n: Int): PPLProperty = {
          val pplbox = new Double_Box(n, Degenerate_Element.EMPTY)
          new PPLProperty(pplbox)
        }
      }
    }

    val PPLTypeSymbol = implicitly[c.WeakTypeTag[PPLType]].tpe.typeSymbol

    // Here we substitute the placeholder Double_Box symbol with the real type
    val classesWithSubstitution = classes.tree.substituteSymbols(
      List(typeOf[Double_Box].typeSymbol),
      List(PPLTypeSymbol))

    // Here we add the resulting domain as output of the tree
    val outputTree = classesWithSubstitution match {
      case Block(stats, expr) => Block(stats, Ident(newTermName("PPLProperty")))
    }

    c.Expr[NumericalDomain](outputTree)
  }
}
