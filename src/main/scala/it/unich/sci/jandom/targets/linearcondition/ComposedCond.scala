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

package it.unich.sci.jandom
package targets.linearcondition

import domains.NumericalProperty

/**
 * This is the class for the logical and of two conditions.
 * @param cond1 the first condition
 * @param cond2 the second condition
 * @return the logical "and" of cond1 and cond2
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class AndCond(cond1: LinearCond, cond2: LinearCond) extends LinearCond {
  def opposite = new OrCond(cond1.opposite, cond2.opposite)  
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = cond2.analyze(cond1.analyze(input))
  override def toString = "(" + cond1 + " && " + cond2 + ")"
}

/**
 * This is the class for the logical or of two conditions.
 * @param cond1 the first condition
 * @param cond2 the second condition
 * @return the logical "or" of cond1 and cond2
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class OrCond(cond1: LinearCond, cond2: LinearCond) extends LinearCond {
  def opposite = new AndCond(cond1.opposite, cond2.opposite)
  
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = 
    cond1.analyze(input) union cond2.analyze(input)  
  override def toString = "(" + cond1 + " || " + cond2 + ")"
}

/**
 * This is the class for the logical not of a condition.
 * @param cond the original condition
 * @return the logical "not" of cond
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
case class NotCond(cond: LinearCond) extends LinearCond {
  def opposite = cond
  override def analyze[Property <: NumericalProperty[Property]] (input: Property): Property = cond.opposite.analyze(input)
  override def toString = "!("+cond+")"
}
