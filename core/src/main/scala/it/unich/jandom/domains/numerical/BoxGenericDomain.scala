/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.domains.numerical

trait BoxGenericDomain[A]/*[DOM <: BoxGenericDomain[DOM]]*/ extends NumericalDomain {
  type Property <: NumericalProperty[Property] {
    def high: Array[A]
    def low: Array[A]
    def linearInequality(lf: LinearForm): Property
    def linearAssignment(n: Int, lf: LinearForm): Property
    def linearEvaluation(lf: LinearForm): (A, A)
  }

  def makeBox(low: Array[A], high: Array[A], isEmpty: Boolean): Property
}
