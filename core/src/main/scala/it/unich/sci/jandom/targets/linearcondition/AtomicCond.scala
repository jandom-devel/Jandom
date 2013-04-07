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
package targets.linearcondition

import targets.LinearForm
import domains.NumericalProperty

/**
 * The class for atomic conditions of the kind \vec c * \vec x <=> 0.
 * @param lf the linear form on the left hand size of the inequation
 * @param op the relation operator of the atomic condition
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class AtomicCond[T](lf: LinearForm[T], op: AtomicCond.ComparisonOperators.Value) (implicit numeric: Numeric[T]) extends LinearCond {
  import numeric._;
 
  /**
   * This method returns the homogeneous coefficient of lf as an array of doubles... it is for internal use only,
   * and should be removed someday, since it does not allow linear forms over different fields.
   */
  private def homcoeff(lf: LinearForm[T]) = lf.homcoeff.map { _.toDouble }.toArray
  
  /**
   * This method returns the inhomogeneous as a double... it is for internal use only,
   * and should be removed someday, since it does not allow linear forms over different fields.
   */
  private def known(lf: LinearForm[T]) = lf.known.toDouble
  
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = op match {    
    case AtomicCond.ComparisonOperators.LTE => input.linearInequality( homcoeff(lf), known(lf) )
    case AtomicCond.ComparisonOperators.LT => input.linearInequality( homcoeff(lf), known(lf) )
    case AtomicCond.ComparisonOperators.GTE => input.linearInequality( homcoeff(-lf), known(-lf) )
    case AtomicCond.ComparisonOperators.GT => input.linearInequality( homcoeff(-lf), known(-lf) )
    case AtomicCond.ComparisonOperators.NEQ => input.linearDisequality( homcoeff(lf), known(lf) )
    case AtomicCond.ComparisonOperators.EQ => input.linearInequality( homcoeff(lf), known(lf) ) intersection 
    	input.linearInequality( homcoeff(-lf), known(-lf) )    
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
