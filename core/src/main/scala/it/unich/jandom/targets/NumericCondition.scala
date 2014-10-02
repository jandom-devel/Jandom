/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.targets

import it.unich.jandom.domains.numerical.NumericalProperty

/**
 * This is the abstract class for all numeric conditions. Each condition has an opposite method
 * which returns the opposite condition, and an analyze method which performs static analysis.
 * @author Gianluca Amato <gamato@unich.it>
 */
sealed abstract class NumericCondition {
  /**
   * The analyzer for linear conditions.
   * @param input the property which holds
   * @return the property given by the logical and of input and the condition itself
   */
  def analyze[Property <: NumericalProperty[Property]](input: Property): Property

  /**
   * Returns the opposite linear condition (the one obtained by reversing the order of inequalities)
   * @return the opposite linear condition
   */
  val opposite: NumericCondition

  /**
   * Return the dimension of the linear condition.
   */
  val dimension: Int

  /**
   * Returns the textual representation of the condition, with the provided variable names.
   * @param vars symbolic names of variables in the condition.
   */
  def mkString(vars: Seq[String]): String

  /**
   * @inheritdoc
   * It is equivalent to `mkString` with variable names `v0`...`vn`
   */
  override def toString = mkString(Stream.from(0).map { "v" + _ })
}

object NumericCondition {

  /**
   * The comparison operators.
   */
  object ComparisonOperators extends Enumeration {
    val EQ = Value("==")
    val GT = Value(">")
    val GTE = Value(">=")
    val LT = Value("<")
    val LTE = Value("<=")
    val NEQ = Value("!=")

    /**
     * Returns the opposite comparison symbol.
     * @return the opposite comparison symbol
     */
    def opposite(v: Value): Value = {
      v match {
        case EQ => NEQ
        case GT => LTE
        case GTE => LT
        case LT => GTE
        case LTE => GT
        case NEQ => EQ
      }
    }

    /**
     * Returns the negated comparison symbol. The negated comparison symbol is the
     * one obtained when reading it right-to-left.
     * @return the negated comparison symbol
     */
    def negate(v: Value): Value = {
      v match {
        case EQ => EQ
        case GT => LT
        case GTE => LTE
        case LT => GT
        case LTE => GTE
        case NEQ => NEQ
      }
    }
  }

  /**
   * The class for atomic conditions of the kind \vec c * \vec x <=> 0.
   * @param lf the linear form on the left hand size of the inequation
   * @param op the relation operator of the atomic condition
   * @author Gianluca Amato <gamato@unich.it>
   *
   */
  case class AtomicCond(numexpr: NumericExpression, op: ComparisonOperators.Value) extends NumericCondition {

    override def analyze[Property <: NumericalProperty[Property]](input: Property): Property = op match {
      case ComparisonOperators.LTE => numexpr.lteZero(input)
      case ComparisonOperators.LT => numexpr.ltZero(input)
      case ComparisonOperators.GTE => (-numexpr).lteZero(input)
      case ComparisonOperators.GT => (-numexpr).ltZero(input)
      case ComparisonOperators.NEQ => numexpr.neqZero(input)
      case ComparisonOperators.EQ => (-numexpr).lteZero(numexpr.lteZero(input))
    }

    lazy val opposite = new AtomicCond(numexpr, ComparisonOperators.opposite(op))

    lazy val dimension = numexpr.dimension

    override def mkString(vars: Seq[String]) = s"${numexpr.mkString(vars)}${op}0"
  }

  /**
   * The companion object for the AtomicCond class.
   *
   * It contains additional factories.
   */
  object AtomicCond {
    def apply(e1: NumericExpression, op: ComparisonOperators.Value, e2: NumericExpression): AtomicCond = {
      if (e2.isZero)
        AtomicCond(e1, op)
      else if (e1.isZero)
        AtomicCond(e2, ComparisonOperators.negate(op))
      else
        AtomicCond(e1 - e2, op)
    }
  }

  /**
   * This is the class for the logical and of two conditions.
   * @param cond1 the first condition
   * @param cond2 the second condition
   * @return the logical "and" of cond1 and cond2
   * @author Gianluca Amato <gamato@unich.it>
   *
   */
  case class AndCond(cond1: NumericCondition, cond2: NumericCondition) extends NumericCondition {
    lazy val opposite = new OrCond(cond1.opposite, cond2.opposite)
    override def analyze[Property <: NumericalProperty[Property]](input: Property): Property = cond2.analyze(cond1.analyze(input))
    override def mkString(vars: Seq[String]) = "(" + cond1.mkString(vars) + " && " + cond2.mkString(vars) + ")"
    val dimension = cond1.dimension max cond2.dimension
  }

  /**
   * This is the class for the logical or of two conditions.
   * @param cond1 the first condition
   * @param cond2 the second condition
   * @return the logical "or" of cond1 and cond2
   * @author Gianluca Amato <gamato@unich.it>
   *
   */
  case class OrCond(cond1: NumericCondition, cond2: NumericCondition) extends NumericCondition {
    lazy val opposite = new AndCond(cond1.opposite, cond2.opposite)
    override def analyze[Property <: NumericalProperty[Property]](input: Property): Property =
      cond1.analyze(input) union cond2.analyze(input)
    override def mkString(vars: Seq[String]) = "(" + cond1.mkString(vars) + "||" + cond2.mkString(vars) + ")"
    val dimension = cond1.dimension max cond2.dimension
  }

  /**
   * This is the class for the logical not of a condition.
   * @param cond the original condition
   * @return the logical "not" of cond
   * @author Gianluca Amato <gamato@unich.it>
   *
   */
  case class NotCond(cond: NumericCondition) extends NumericCondition {
    val opposite = cond
    override def analyze[Property <: NumericalProperty[Property]](input: Property): Property = cond.opposite.analyze(input)
    override def mkString(vars: Seq[String]) = "!(" + cond.mkString(vars) + ")"
    val dimension = cond.dimension
  }

  /**
   * An atomic condition for a non-deterministic choice.
   * @author Gianluca Amato <gamato@unich.it>
   */
  object BRandomCond extends NumericCondition {
    val opposite = BRandomCond
    override def analyze[Property <: NumericalProperty[Property]](input: Property) = input
    override def mkString(vars: Seq[String]) = "brandom()"
    val dimension = 0
  }

  /**
   * The valid condition.
   * @author Gianluca Amato <gamato@unich.it>
   */
  object TrueCond extends NumericCondition {
    val opposite = FalseCond
    override def analyze[Property <: NumericalProperty[Property]](input: Property) = input
    override def mkString(vars: Seq[String]) = "TRUE"
    val dimension = 0
  }

  /**
   * The false condition.
   * @author Gianluca Amato <gamato@unich.it>
   */
  object FalseCond extends NumericCondition {
    val opposite = TrueCond
    override def analyze[Property <: NumericalProperty[Property]](input: Property): Property = input.bottom
    override def mkString(vars: Seq[String]) = "FALSE"
    val dimension = 0
  }

}