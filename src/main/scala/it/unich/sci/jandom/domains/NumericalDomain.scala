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

package it.unich.sci.jandom
package domains

import it.unich.sci.jandom.parameters.Parameter
import it.unich.sci.jandom.parameters.ParameterValue
import scala.collection.SortedSet
import parma_polyhedra_library.Parma_Polyhedra_Library
import parma_polyhedra_library.Octagonal_Shape_double

/**
 * Base trait for numerical domains. A numerical domain is a factory for numerical properties. It
 * should be used as a base class for companion objects of the descendants of
 * [[it.unich.sci.domains.jandom.NumericalProperty]].
 * @tparam Property the property class corresponding to this domain
 * @author Gianluca Amato <g.amato@unich.it>
 */
trait NumericalDomain[Property <: NumericalProperty[Property]] {

  /**
   * Create an abstract property representing the full n-dimensional space.
   * @param n the dimension of the environment space.
   * @return the full n-dimensional space.
   */
  def full(n: Int): Property

  /**
   * Create an abstract property representing the empty n-dimensional space.
   * @param n the dimension of the environment space.
   * @return the empty n-dimensional space.
   */
  def empty(n: Int): Property
}

object NumericalDomain extends Parameter[NumericalDomain[T] forSome { type T <: NumericalProperty[T]}] {
  val name = "Domain"
  val description = "The numerical domain to use for the analysis."
  val enabledValues = Seq[NumericalDomain[T] with ParameterValue forSome { type T <: NumericalProperty[T]}](
      BoxDouble, ParallelotopeBreeze, PPLBoxDouble, PPLCPolyhedron, new PPLDomain[Octagonal_Shape_double])
}
