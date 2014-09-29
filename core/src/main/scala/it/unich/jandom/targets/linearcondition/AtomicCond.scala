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

package it.unich.jandom.targets.linearcondition

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.targets.NumericExpression

/**
 * The class for atomic conditions of the kind \vec c * \vec x <=> 0.
 * @param lf the linear form on the left hand size of the inequation
 * @param op the relation operator of the atomic condition
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
case class AtomicCond(numexpr: NumericExpression, op: AtomicCond.ComparisonOperators.Value) extends LinearCond {

  override def analyze[Property <: NumericalProperty[Property]](input: Property): Property = op match {
    case AtomicCond.ComparisonOperators.LTE => numexpr.lteZero(input)
    case AtomicCond.ComparisonOperators.LT => numexpr.ltZero(input)
    case AtomicCond.ComparisonOperators.GTE => (-numexpr).lteZero(input)
    case AtomicCond.ComparisonOperators.GT => (-numexpr).ltZero(input)
    case AtomicCond.ComparisonOperators.NEQ => numexpr.neqZero(input)
    case AtomicCond.ComparisonOperators.EQ => (-numexpr).lteZero(numexpr.lteZero(input))
  }

  lazy val opposite = new AtomicCond(numexpr, AtomicCond.ComparisonOperators.opposite(op))

  lazy val dimension = numexpr.dimension

  override def mkString(vars: Seq[String]) = s"${numexpr.mkString(vars)}${op}0"
}

/**
 * The companion object for the AtomicCond class.
 *
 * It contains the Enumeration of the comparison/relational operators.
 */
object AtomicCond {

  def apply(e1: NumericExpression, op: AtomicCond.ComparisonOperators.Value, e2: NumericExpression): AtomicCond = {
    if (e2.isZero)
      AtomicCond(e1, op)
    else if (e1.isZero)
      AtomicCond(e2, ComparisonOperators.negate(op))
    else
      AtomicCond(e1 - e2, op)
  }

  /**
   * The comparison operator.
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
}
