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

package it.unich.sci.jandom
package domains

import language.experimental.macros
import scala.reflect.macros.Context
import it.unich.sci.jandom.ui.ParameterValue

/**
 * This class containts macros for compile-time creation of PPL backed
 * numerical properties.
 */
object PPLPropertyMacros {

  def PPLDomain[PPLType]: NumericalDomain with ParameterValue = macro PPLDomainImpl[PPLType]

  def PPLDomainImpl[PPLType: c.WeakTypeTag](c: Context): c.Expr[NumericalDomain with ParameterValue] = {
    import c.universe._
    import parma_polyhedra_library.Double_Box
        
    val PPLTypeSymbol = implicitly[c.WeakTypeTag[PPLType]].tpe.typeSymbol
    val PPLTypeName = c.literal(PPLTypeSymbol.name.toString)

    val classes = reify {
      import utils.PPLUtils
      import parma_polyhedra_library.Linear_Expression
      import parma_polyhedra_library.Linear_Expression_Coefficient
      import parma_polyhedra_library.Linear_Expression_Variable
      import parma_polyhedra_library.Variable
      import parma_polyhedra_library.Coefficient
      import parma_polyhedra_library.Relation_Symbol
      import parma_polyhedra_library.Constraint
      import parma_polyhedra_library.Degenerate_Element
      import parma_polyhedra_library.Variables_Set

      /**
       * The domain for possibly opened box over doubles implemented within $PPL. This is essentially
       * a wrapper transforming methods of `Double_Box` to methods of `NumericalProperty`. We clone
       * objects in order have an immutable class.
       * @param pplbox an object of class `Double_Box` which is the $PPL wrapped object.
       * @author Gianluca Amato <amato@sci.unich.it>
       */
      class PPLProperty(private val pplbox: Double_Box) extends NumericalProperty[PPLProperty] {

        def widening(that: PPLProperty): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          newpplbox.upper_bound_assign(that.pplbox)
          newpplbox.widening_assign(pplbox, null)
          new PPLProperty(newpplbox)
        }

        def narrowing(that: PPLProperty): PPLProperty = {
          val newpplbox = new Double_Box(that.pplbox)
          newpplbox.CC76_narrowing_assign(pplbox)
          new PPLProperty(newpplbox)
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

        def linearAssignment(n: Int, coeff: Array[Double], known: Double): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          newpplbox.affine_image(new Variable(n), PPLUtils.toPPLLinearExpression(coeff, known), new Coefficient(1))
          new PPLProperty(newpplbox)
        }

        def linearInequality(coeff: Array[Double], known: Double): PPLProperty = {
          val le = PPLUtils.toPPLLinearExpression(coeff, known)
          val newpplbox = new Double_Box(pplbox)
          newpplbox.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
          new PPLProperty(newpplbox)
        }

        def linearDisequality(coeff: Array[Double], known: Double): PPLProperty = {
          throw new IllegalAccessException("Unimplemented feature");
        }

        def addDimension: PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          newpplbox.add_space_dimensions_and_project(1)
          new PPLProperty(newpplbox)
        }

        def delDimension(n: Int): PPLProperty = {
          val newpplbox = new Double_Box(pplbox)
          val dims = new Variables_Set
          dims.add(new Variable(n))
          newpplbox.remove_space_dimensions(dims)
          new PPLProperty(newpplbox)
        }

        def dimension: Int = pplbox.space_dimension.toInt

        def isEmpty: Boolean = pplbox.is_empty

        def isFull: Boolean = pplbox.is_universe

        def empty() = PPLProperty.empty(pplbox.space_dimension.toInt)

        def full() = PPLProperty.full(pplbox.space_dimension.toInt)

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

        def mkString(vars: IndexedSeq[String]): Seq[String] =
          PPLUtils.replaceOutputWithVars(pplbox.toString, vars)
      }

      /**
       * This is the factory for ``PPLProperty`` properties.
       */
      object PPLProperty extends NumericalDomain with ParameterValue {
        PPLInitializer

        type Property = PPLProperty

        val name = s"PPL ${PPLTypeName.splice}"
        val description = s"A domain which uses the class ${PPLTypeName.splice} of the PPL library."

        def full(n: Int): PPLProperty = {
          val pplbox = new Double_Box(n, Degenerate_Element.UNIVERSE)
          new PPLProperty(pplbox)
        }

        def empty(n: Int): PPLProperty = {
          val pplbox = new Double_Box(n, Degenerate_Element.EMPTY)
          new PPLProperty(pplbox)
        }
      }
    }

    // This was used in a previous version of the macro, when domains where based on 
    // existential types. We have left the AST here in case it is needed again.
    // val typetree = Typed(Ident(newTermName("PPLProperty")), ExistentialTypeTree(CompoundTypeTree(Template(List(AppliedTypeTree(Ident(newTypeName("NumericalDomain")), List(Ident(newTypeName("X")))), Ident(newTypeName("ParameterValue"))), emptyValDef, List())), List(TypeDef(Modifiers(Flag.DEFERRED), newTypeName("X"), List(), TypeBoundsTree(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Nothing")), AppliedTypeTree(Ident(newTypeName("NumericalProperty")), List(Ident(newTypeName("X")))))))))

    // Here we substitute the placeholder Double_Box symbol with the real type
    val classesWithSubstitution = classes.tree.substituteSymbols(
      List(typeOf[Double_Box].typeSymbol),
      List(PPLTypeSymbol))

    // Here we add the resulting domain as output of the tree
    val outputTree = classesWithSubstitution match {
      case Block(stats, expr) => Block(stats, Ident(newTermName("PPLProperty")))
    }

    c.Expr[NumericalDomain with ParameterValue](outputTree)
  }
}
