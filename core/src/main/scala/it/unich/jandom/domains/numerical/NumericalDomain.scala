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

package it.unich.jandom.domains.numerical

import it.unich.jandom.domains.DimensionFiberedDomain

/**
 * Base class for numerical domains. The fiber of dimension `n` of a numerical domain
 * is an abstraction of the powerset of R^n. Numerical properties are  instances of
 * [[it.unich.jandom.domains.NumericalProperty]].
 * @author Gianluca Amato <gamato@unich.it>
 */
abstract class NumericalDomain extends DimensionFiberedDomain {
  /**
   * @inheritdoc
   * For numerical domains, properties needs to be instances of [[it.unich.jandom.domains.NumericalProperty]].
   */
  type Property <: NumericalProperty[Property]
}
