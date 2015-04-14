/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.objectmodels

/**
 * A TreeObjectModel is an object model where the subtype relationships
 * forms a forest instead of a graph.
 * @author Gianluca Amato <gamato@unich.it>
 */

trait TreeObjectModel extends ObjectModel {

  /**
   * It determines the glb's of two types. It is only present at this point of the hierarchy because
   * it is easy to compute it from the ordering relation for tree hierarchies, not as much for
   * general hierarchies.
   */
  def glb(t1: Type, t2: Type) = {
    if (lteq(t1, t2))
      Option(t1)
    else if (lteq(t2, t1))
      Option(t2)
    else None
  }
}
