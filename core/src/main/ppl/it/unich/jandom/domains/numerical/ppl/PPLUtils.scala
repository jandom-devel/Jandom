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

package it.unich.jandom.domains.numerical.ppl

import it.unich.jandom.domains.numerical.LinearForm

import parma_polyhedra_library._

/**
 * This is a collection of methods used by the PPL-based numerical domains.
 * @author Gianluca Amato <gamato@unich.it>
 */
private[jandom] object PPLUtils {
  /**
   * Converts a `LinearForm` into a pair made of a `Linear_Expression` object and a
   * `Coefficient` object, which is the denumerator to be used in linear assignments.
   * @param coeff the homogeneous coefficients.
   * @param known the in-homogeneous coefficient.
   */
  def toPPLLinearExpression(lf: LinearForm[Double]): (Linear_Expression, Coefficient) = {
    if (lf.toPPL != null)
      lf.toPPL.asInstanceOf[(Linear_Expression, Coefficient)]
    else {
      val coeffs = lf.coeffs map { BigDecimal.exact(_) }
      val maxScale = (coeffs map { _.scale }).max
      val denumerator = BigDecimal(10) pow maxScale
      val newcoeffs = coeffs map { (x: BigDecimal) => (x * denumerator).toBigIntExact.get.bigInteger }
      var le: Linear_Expression = new Linear_Expression_Coefficient(new Coefficient(newcoeffs(0)))
      for (i <- 0 until lf.dimension) {
        le = le.sum((new Linear_Expression_Variable(new Variable(i)).times(new Coefficient(newcoeffs(i + 1)))))
      }
      val result = (le, new Coefficient(denumerator.toBigIntExact.get.bigInteger))
      lf.toPPL = result
      result
    }
  }

  /**
   * Converts a PPL linear expression couple with a coefficient for the denominator into a LinearForm.
   */
  def fromPPLExpression(e: Linear_Expression): LinearForm[Double] = {
	 e match {
	   case e: Linear_Expression_Coefficient => LinearForm.c(e.argument().getBigInteger().doubleValue())
	   case e: Linear_Expression_Difference => fromPPLExpression(e.left_hand_side()) - fromPPLExpression(e.right_hand_side())
	   case e: Linear_Expression_Sum => fromPPLExpression(e.left_hand_side()) + fromPPLExpression(e.right_hand_side())
	   case e: Linear_Expression_Times => fromPPLExpression(e.linear_expression()) * e.coefficient().getBigInteger().doubleValue()
	   case e: Linear_Expression_Unary_Minus => - fromPPLExpression(e.argument())
	   case e: Linear_Expression_Variable => LinearForm.v(e.argument().id().toInt)
	 }
  }

  /**
   * Converts a PPL Constraints into a sequence of LinearForms. The conversion in only
   * approximate since we cannot represent open constraints.
   */
  def fromPPLConstraint(c: Constraint): Seq[LinearForm[Double]] = {
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
    import collection.JavaConversions._

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