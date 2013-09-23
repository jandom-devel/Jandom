/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.sci.jandom.domains

/**
 * A trait which implements caching of top and bottom values in a
 * dimension fibered domain.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait CachedTopBottom extends DimensionFiberedDomain {
  /**
   * This is the cache for bottom properties.
   */
  private var cacheBottom: Map[Int, Property] = Map()

  abstract override def bottom(n: Int): Property = {
    if (!(cacheBottom isDefinedAt n))
      cacheBottom += (n -> super.bottom(n))
    cacheBottom(n)
  }

  /**
   * This is the cache for top properties.
   */
  private var cacheTop: Map[Int, Property] = Map()

  abstract override def top(n: Int): Property = {
    if (!(cacheTop isDefinedAt n))
      cacheTop += (n -> super.top(n))
    cacheTop(n)
  }

}
