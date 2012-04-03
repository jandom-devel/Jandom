/* JANDOM is free software: you can redistribute it and/or modify
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
 * (c) 2011,2012 Gianluca Amato
 */

package it.unich.sci.jandom.domains

import it.unich.sci.jandom.widenings.Widening
import it.unich.sci.jandom.narrowings.Narrowing

/** 
 * Trait for numerical domains. A numerical domains should produce numerical properties. It
 * should be used as a base class for companion objects of NumericalProperties.
 * @tparam Property the property class corresponding to this domain
 * @author Gianluca Amato <g.amato@unich.it>
 */
trait NumericalDomain[Property <: NumericalProperty[Property]] {
  
  /**
   * The standard widening for the domain
   */
  val widening = new Widening[Property] {
    def apply(current: Property, next: Property) = current.widening(next)
  }
  
  /**
   * The standard narrowing for the domain
   */
  val narrowing = new Narrowing[Property] {
    def apply(current: Property, next: Property) = current.narrowing(next)
  }
  
  /**
   * Create an abstract property representing the full n-dimensional space.
   * @param n the dimension of the environment space.
   * @return the full n-dimensional environment space.
   */
  def full(n:Int): Property
  
  /**
   * Create an abstract property representing the empty n-dimensional space.
   * @param n the dimension of the environment space.
   * @return the empty n-dimensional environment space.
   */
  def empty(n:Int): Property
  
}
