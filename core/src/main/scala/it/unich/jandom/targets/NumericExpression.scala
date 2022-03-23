/**
 * Copyright 2014, 2016 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.targets

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.LinearForm._
import it.unich.jandom.targets.NumericExpression._
import spire.math.Rational

/**
 * This is the root of the hierarchy of all numeric expressions. Concrete instances
 * are in the companion object. It implements operation for composing expressions.
 * @author Gianluca Amato <gamato@unich.it>
 */
sealed abstract class NumericExpression {
  /**
   * This method analyzes a numeric expression, by adding a new dimension for the result.
   * @param input the abstract property used to evaluate the expression.
   * @return the resulting proprerty with a new dimension for the evaluated expression.
   */
  def analyze[Property <: NumericalProperty[Property]](input: Property): Property

  /**
   * This method analyzes a numeric expression, and assign the result to variable `v`
   * @param input the abstract property where the expression is evaluated.
   * @param v the variable which is the target of the assignment.
   * @return the resulting property with a new dimension for the evaluated expression.
   */
  def assignTo[Property <: NumericalProperty[Property]](v: Int)(input: Property): Property

  /**
   * This methods returns the subset of input where the expression is less or equal to 0.
   */
  def lteZero[Property <: NumericalProperty[Property]](input: Property): Property = {
    val lf = LinearForm.v(input.dimension)
    analyze(input).linearInequality(lf).delVariable()
  }

  /**
   * This methods returns the subset of input where the expression is strictly less than 0.
   * At the moment it is equivalent to lteZero since we do not support strict contraints.
   */
  def ltZero[Property <: NumericalProperty[Property]](input: Property): Property = {
    val lf = LinearForm.v(input.dimension)
    analyze(input).linearInequality(lf).delVariable()
  }

  /**
   * This methods returns the subset of input where the expression is different from 0.
   */
  def neqZero[Property <: NumericalProperty[Property]](input: Property): Property = {
    val lf = LinearForm.v(input.dimension)
    analyze(input).linearDisequality(lf).delVariable()
  }

  /**
   * Returns the dimension of the expression, i.e. the greatest variable index which
   * occurs in the expression plus one.
   */
  def dimension: Int

  /**
   * Returns the textual representation of an expression.
   * @param vars symbolic names of variables in the expression.
   */
  def mkString(vars: Seq[String]): String

  /**
   * @inheritdoc
   * It is equivalent to `mkString` with variable names `v0`...`vn`.
   */
  override def toString = mkString(LazyList.from(0).map { "v" + _ })

  /**
   * Returns true if the expression is syntactically zero
   */
  def isZero: Boolean

  def unary_- : NumericExpression = UnaryMinusExpression(this)
  def +(expr: NumericExpression): NumericExpression = AddExpression(this, expr)
  def -(expr: NumericExpression): NumericExpression = SubExpression(this, expr)
  def *(expr: NumericExpression): NumericExpression = MulExpression(this, expr)
  def /(expr: NumericExpression): NumericExpression = DivExpression(this, expr)
}

/**
 * This is the companion class for NumericExpression, and contains all the concrete
 * implementation of the abstract class.
 * @author Gianluca Amato <gamato@unich.it>
 */
object NumericExpression {
  import scala.language.implicitConversions

  /**
   * Object for non-deterministic expression.
   */
  case object NonDeterministicExpression extends NumericExpression {
    def analyze[Property <: NumericalProperty[Property]](input: Property): Property =
      input.addVariable()

    def assignTo[Property <: NumericalProperty[Property]](v: Int)(input: Property): Property =
      input.nonDeterministicAssignment(v)

    override def lteZero[Property <: NumericalProperty[Property]](input: Property): Property =
      input

    override def ltZero[Property <: NumericalProperty[Property]](input: Property): Property =
      input

    override def neqZero[Property <: NumericalProperty[Property]](input: Property): Property =
      input

    def mkString(vars: Seq[String]) = "?"

    def isZero = false

    def dimension = 0
  }

  /**
   * A class for linear expression. Linear expressions are important since they may be
   * analyzed easily. For this reason, operators `+` and `-` try to produce a linear expression
   * as a result when it is possible.
   */
  case class LinearExpression(val lf: LinearForm) extends NumericExpression {

    def analyze[Property <: NumericalProperty[Property]](input: Property): Property =
      input.addVariable().linearAssignment(input.dimension, lf)

    def assignTo[Property <: NumericalProperty[Property]](v: Int)(input: Property): Property =
      input.linearAssignment(v, lf)

    override def lteZero[Property <: NumericalProperty[Property]](input: Property): Property =
      input.linearInequality(lf)

    override def ltZero[Property <: NumericalProperty[Property]](input: Property): Property =
      input.linearInequality(lf)

    override def neqZero[Property <: NumericalProperty[Property]](input: Property): Property =
      input.linearDisequality(lf)

    override def +(expr: NumericExpression) = expr match {
      case expr: LinearExpression => LinearExpression(lf + expr.lf)
      case _ => AddExpression(this, expr)
    }

    override def -(expr: NumericExpression) = expr match {
      case expr: LinearExpression => LinearExpression(lf - expr.lf)
      case _ => SubExpression(this, expr)
    }

    override def *(expr: NumericExpression) = expr match {
      case expr: LinearExpression if (lf.isConstant) =>
        LinearExpression(expr.lf * lf.known)
      case expr: LinearExpression if (expr.lf.isConstant) =>
        LinearExpression(lf * expr.lf.known)
      case _ => MulExpression(this, expr)
    }

    override def /(expr: NumericExpression) = expr match {
      case expr: LinearExpression if (expr.lf.isConstant) =>
        LinearExpression(lf / expr.lf.known)
      case _ => DivExpression(this, expr)
    }

    override def unary_- = LinearExpression(-lf)

    def isZero = lf.isZero

    def dimension = lf.dimension

    def mkString(vars: Seq[String]) = lf.mkString(vars)
  }

  /**
   * A class for the negation of a basic expression.
   */
  case class UnaryMinusExpression(val e: NumericExpression) extends NumericExpression {
    def analyze[Property <: NumericalProperty[Property]](input: Property): Property =
      e.analyze(input).variableNeg()

    def assignTo[Property <: NumericalProperty[Property]](v: Int)(input: Property): Property =
      e.assignTo(v)(input).variableNeg(v)

    def dimension = e.dimension

    def isZero = false

    def mkString(vars: Seq[String]) = s"(- ${e.mkString(vars)})"
  }

  /**
   * A class for the sum of two basic expressions.
   */
  case class AddExpression(val e1: NumericExpression, val e2: NumericExpression) extends NumericExpression {
    def analyze[Property <: NumericalProperty[Property]](input: Property): Property =
      (e2 analyze (e1 analyze input)).variableAdd().delVariable()

    def assignTo[Property <: NumericalProperty[Property]](v: Int)(input: Property): Property =
      e1.assignTo(v)(e2.analyze(input)).variableAdd(v, input.dimension).delVariable()

    def dimension = e1.dimension max e2.dimension

    def isZero = false

    def mkString(vars: Seq[String]) = s"(${e1.mkString(vars)} + ${e2.mkString(vars)})"
  }

  /**
   * A class for the difference of two basic expressions.
   */
  case class SubExpression(val e1: NumericExpression, val e2: NumericExpression) extends NumericExpression {
    def analyze[Property <: NumericalProperty[Property]](input: Property): Property =
      (e2 analyze (e1 analyze input)).variableSub().delVariable()

    def assignTo[Property <: NumericalProperty[Property]](v: Int)(input: Property): Property =
      e1.assignTo(v)(e2.analyze(input)).variableSub(v, input.dimension).delVariable()

    def dimension = e1.dimension max e2.dimension

    def isZero = false

    def mkString(vars: Seq[String]) = s"(${e1.mkString(vars)} - ${e2.mkString(vars)})"
  }

  /**
   * A class for the product of two basic expressions.
   */
  case class MulExpression(val e1: NumericExpression, val e2: NumericExpression) extends NumericExpression {
    def analyze[Property <: NumericalProperty[Property]](input: Property): Property =
      (e2 analyze (e1 analyze input)).variableMul().delVariable()

    def assignTo[Property <: NumericalProperty[Property]](v: Int)(input: Property): Property =
      e1.assignTo(v)(e2.analyze(input)).variableMul(v, input.dimension).delVariable()

    def dimension = e1.dimension max e2.dimension

    def isZero = false

    def mkString(vars: Seq[String]) = s"(${e1.mkString(vars)} * ${e2.mkString(vars)})"
  }

  /**
   * A class for the quotient of two basic expressions.
   */
  case class DivExpression(val e1: NumericExpression, val e2: NumericExpression) extends NumericExpression {
    def analyze[Property <: NumericalProperty[Property]](input: Property): Property =
      (e2 analyze (e1 analyze input)).variableDiv().delVariable()

    def assignTo[Property <: NumericalProperty[Property]](v: Int)(input: Property): Property =
      e1.assignTo(v)(e2.analyze(input)).variableDiv(v, input.dimension).delVariable()

    def dimension = e1.dimension max e2.dimension

    def isZero = false

    def mkString(vars: Seq[String]) = s"(${e1.mkString(vars)} / ${e2.mkString(vars)})"
  }

  /**
   * Implicit conversion from rational constants  to a NumericExpression.
   */
  implicit def ConstantExpression(c: Rational) = LinearExpression(c)

  /**
   * Implicit conversion from constants to a NumericExpression.
   */
  implicit def ConstantExpression[T](c: T)(implicit ev: T => Rational) = LinearExpression(c)

  /**
   * Constructs an expression corresponding to the variable `v`.
   */
  def VariableExpression(v: Int) = LinearExpression(LinearForm.v(v))

  /**
   * Implicit conversion from LinearForm to NumericExpression.
   */
  implicit def linearFormToLinearExpression(lf: LinearForm) = LinearExpression(lf)
}
