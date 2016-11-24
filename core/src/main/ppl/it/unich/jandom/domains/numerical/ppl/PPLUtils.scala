/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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
import parma_polyhedra_library._
import spire.math.Rational
import spire.math.SafeLong

/**
 * This is a collection of methods used by the PPL-based numerical domains.
 * @author Gianluca Amato <gamato@unich.it>
 */
private[jandom] object PPLUtils {
  /**
   * Converts a `LinearForm` into a pair made of a `Linear_Expression` object and a
   * `Coefficient` object, which is the denominator to be used in linear assignments.
   */
  def toPPLLinearExpression(lf: LinearForm): (Linear_Expression, Coefficient) = {
    if (lf.toPPL != null)
      lf.toPPL.asInstanceOf[(Linear_Expression, Coefficient)]
    else {
      val denominator = lf.coeffs.foldLeft(SafeLong.one) { _ * _.denominator }
      val newcoeffs = lf.coeffs map { x => (x.numerator * denominator / x.denominator).toBigInt.bigInteger }
      val leKnown = new Linear_Expression_Coefficient(new Coefficient(newcoeffs.head))
      val le = newcoeffs.tail.zipWithIndex.foldLeft[Linear_Expression](leKnown) {
        (expr, term) => expr.sum((new Linear_Expression_Variable(new Variable(term._2)).times(new Coefficient(term._1))))
      }
      val result = (le, new Coefficient(denominator.toBigInt.bigInteger))
      lf.toPPL = result
      result
    }
  }

  /**
   * Converts a PPL linear expression into a LinearForm.
   */
  def fromPPLExpression(e: Linear_Expression): LinearForm = {
    e match {
      case e: Linear_Expression_Coefficient => LinearForm.c(Rational(e.argument().getBigInteger()))
      case e: Linear_Expression_Difference => fromPPLExpression(e.left_hand_side()) - fromPPLExpression(e.right_hand_side())
      case e: Linear_Expression_Sum => fromPPLExpression(e.left_hand_side()) + fromPPLExpression(e.right_hand_side())
      case e: Linear_Expression_Times => fromPPLExpression(e.linear_expression()) * e.coefficient().getBigInteger().doubleValue()
      case e: Linear_Expression_Unary_Minus => -fromPPLExpression(e.argument())
      case e: Linear_Expression_Variable => LinearForm.v(e.argument().id().toInt)
    }
  }

  /**
   * Converts a PPL Constraints into a sequence of LinearForms. The conversion is only
   * approximate since we cannot represent open constraints.
   */
  def fromPPLConstraint(c: Constraint): Seq[LinearForm] = {
    val exp = c.left_hand_side().subtract(c.right_hand_side())
    val lf = fromPPLExpression(exp)
    c.kind match {
      case Relation_Symbol.EQUAL => Seq(lf, -lf)
      case Relation_Symbol.LESS_OR_EQUAL | Relation_Symbol.LESS_THAN => Seq(lf)
      case Relation_Symbol.GREATER_THAN | Relation_Symbol.GREATER_OR_EQUAL => Seq(-lf)
      case Relation_Symbol.NOT_EQUAL => Seq()
    }
  }

  /**
   * Determines whether a PPL constraint has an exact representation as a sequence of linear form.
   */
  def isRepresentableAsLinearForms(c: Constraint): Boolean = {
    c.kind match {
      case Relation_Symbol.EQUAL | Relation_Symbol.LESS_OR_EQUAL | Relation_Symbol.GREATER_OR_EQUAL => true
      case _ => false
    }
  }

  /**
   * Generates a string representation of a constraint system.
   * @param cs a constraint system
   * @param vars the variables to use for the string form
   */
  def constraintsToString(cs: Constraint_System, vars: Seq[String]): String = {
    import scala.collection.JavaConversions._

    val vs = new Variable_Stringifier {
      def stringify(x: Long) = vars(x.toInt)
    }
    Variable.setStringifier(vs)
    val result = for (c <- cs) yield c.toString
    Variable.setStringifier(null)
    result.mkString("[ ", " , ", " ]")
  }

  /**
   * Converts a sequence into a partial function.
   * @param rho the original sequence. If `rho(i)=j` and `j>0`, the resulting partial
   * function maps `i` to `j`. If `j=0`, then `i` is not in the domain of the resulting
   * function.
   */
  def sequenceToPartialFunction(rho: Seq[Int]): Partial_Function = {
    val pf = new Partial_Function
    for ((newi, i) <- rho.zipWithIndex; if newi >= 0) {
      pf.insert(i, newi)
    }
    pf
  }
}
