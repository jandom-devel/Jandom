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
package it.unich.sci.jandom.domains

import parma_polyhedra_library.Linear_Expression
import parma_polyhedra_library.Linear_Expression_Coefficient
import parma_polyhedra_library.Linear_Expression_Variable
import parma_polyhedra_library.Variable
import parma_polyhedra_library.Coefficient

/**
 * This is the common trait for all the classes implementing numerical domains with the use of $PPL.
 * @author Gianluca Amato <g.amato@unich.it>
 */
trait PPLNumericalProperty[Property] extends NumericalProperty[Property] {
  type PPLProperty =  {
    def space_dimension(): Long
    def is_empty(): Boolean
    def is_universe(): Boolean
  }
  
  protected val pplobject: PPLProperty

  def dimension: Int = pplobject.space_dimension.toInt

  def isEmpty: Boolean = pplobject.is_empty

  def isFull: Boolean = pplobject.is_universe

  /**
   * Converts a sequence of homogeneous and in-homogeneous coefficients into a `Linear_Expression`, which
   * is a PPL internal object.
   * @param coeff the homogeneous coefficients.
   * @param known the in-homogeneous coefficient.
   */
  private def toPPLLinearExpression(coeff:Array[Double], known:Double): Linear_Expression = {
    var le : Linear_Expression = new Linear_Expression_Coefficient(new Coefficient(known.toInt))
	for (i <- 0 to (coeff.length - 1)) {
	  le = le.sum ( (new Linear_Expression_Variable(new Variable(i)).times(new Coefficient(coeff(i).toInt)) ))
	}
    return le
  }
}
