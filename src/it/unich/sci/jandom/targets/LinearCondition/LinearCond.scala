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

package it.unich.sci.jandom.targets.LinearCondition
import it.unich.sci.jandom.domains.NumericalProperty

/**
 * This is the abstract class for all linear conditions. Each condition has an opposite method
 * which returns the opposite condition, and an analyze method which performs static analysis.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
abstract class LinearCond {
  def analyze[Property <: NumericalProperty[Property]] (input: Property): Property;  
  def opposite : LinearCond;
}