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
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.sci.jandom.domains.objects

import it.unich.sci.jandom.domains.AbstractDomain
import it.unich.sci.jandom.domains.WithBottom
import it.unich.sci.jandom.domains.WithTop
import it.unich.sci.jandom.domains.numerical.NumericalDomain

/**
 * This is the base for all domains which analyze objects abstractly, i.e. without
 * any semantic knowledge of their contents.
 * @author Gianluca Amato <gamato@unich.it>
 */
trait ObjectDomain extends AbstractDomain {
  type Property <: ObjectProperty[Property]

  def initial(numroots: Int): Property
  def top(numroots: Int): Property
  def bottom(numroots: Int): Property
}
