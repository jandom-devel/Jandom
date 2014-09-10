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

/**
 * The class for atomic conditions of the kind \vec c * \vec x <=> 0.
 * @param lf the linear form on the left hand size of the inequation
 * @param op the relation operator of the atomic condition
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class AtomicCond[T](lf: LinearForm[T], op: AtomicCond.ComparisonOperators.Value) (implicit numeric: Numeric[T]) extends LinearCond {
  import numeric._;

  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = op match {
    case AtomicCond.ComparisonOperators.LTE => input.linearInequality( lf.toDouble )
    case AtomicCond.ComparisonOperators.LT => input.linearInequality( lf.toDouble )
    case AtomicCond.ComparisonOperators.GTE => input.linearInequality( -lf.toDouble )
    case AtomicCond.ComparisonOperators.GT => input.linearInequality( -lf.toDouble )
    case AtomicCond.ComparisonOperators.NEQ => input.linearDisequality( lf.toDouble )
    case AtomicCond.ComparisonOperators.EQ => input.linearInequality( lf.toDouble ).linearInequality( -lf.toDouble )
  }

  lazy val opposite = new AtomicCond(lf, AtomicCond.ComparisonOperators.opposite(op))

  val dimension = lf.dimension

  override def mkString(vars: Seq[String]) = lf.mkString(vars) + op + "0"
}

/**
 * The companion object for the AtomicCond class.
 *
 * It contains the Enumeration of the comparison/relational operators.
 */
object AtomicCond {

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
    def opposite(v: Value):Value = {
      return v match {
        case EQ => NEQ
        case GT => LTE
        case GTE => LT
        case LT => GTE
        case LTE => GT
        case NEQ => EQ
      }
    }
  }
}
