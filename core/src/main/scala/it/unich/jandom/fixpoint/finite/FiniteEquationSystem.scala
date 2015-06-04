/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.fixpoint.finite

import it.unich.jandom.fixpoint._
import it.unich.jandom.utils.Relation

/**
 * This is the trait for an equation system with a finite set of unknowns
 * and static dependencies between them. When computing `apply(rho)(x)`,
 * the result may only depend on values of `rho(y)` for an `y` such that 
 * `y infl x`.
 */
trait FiniteEquationSystem extends EquationSystem {
  /**
   * The collection of all unknowns.
   */
  def unknowns: Iterable[Unknown]

  /**
   * A relation between an unknown x and the unknowns y it influences.
   */
  val infl: Relation[Unknown, Unknown]
}
