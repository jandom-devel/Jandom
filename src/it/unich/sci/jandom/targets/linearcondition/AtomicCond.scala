/**
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
 *
 * (c) 2012 Gianluca Amato
 */
package it.unich.sci.jandom.targets.linearcondition

import it.unich.sci.jandom.targets.LinearForm
import it.unich.sci.jandom.domains.NumericalProperty


/**
 * The class for atomic conditions.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class AtomicCond[T](lf: LinearForm[T], op: AtomicCond.ComparisonOperators.Value) (implicit numeric: Numeric[T]) extends LinearCond {
  import numeric._;
 
  private def homcoeff(lf: LinearForm[T]) = lf.homcoeff.map { _.toDouble }.toArray
  private def known(lf: LinearForm[T]) = lf.known.toDouble
    
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = op match {    
    case AtomicCond.ComparisonOperators.LTE => input.linearInequality( homcoeff(lf), known(lf) )
    case AtomicCond.ComparisonOperators.LT => input.linearInequality( homcoeff(lf), known(lf) )
    case AtomicCond.ComparisonOperators.GTE => input.linearInequality( homcoeff(-lf), known(-lf) )
    case AtomicCond.ComparisonOperators.GT => input.linearInequality( homcoeff(-lf), known(-lf) )
    case AtomicCond.ComparisonOperators.NEQ => input.linearDisequality( homcoeff(lf), known(lf) )
    case AtomicCond.ComparisonOperators.EQ => throw new Exception("Not implemented yet")
  }      
  
  def opposite = new AtomicCond(lf, AtomicCond.ComparisonOperators.opposite(op))
  
  override def toString = lf.toString + op + "0"
}


object AtomicCond {
   object ComparisonOperators extends Enumeration {
      val EQ = Value("==")
      val GT = Value(">")
      val GTE = Value(">=")
      val LT = Value("<")
      val LTE = Value("<=")
      val NEQ = Value("!=") 
      
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
