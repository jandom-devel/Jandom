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

package it.unich.jandom.targets

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.domains.numerical.LinearForm

/**
 * This class represent an assignment of linear forms.
 * @tparam T the type of variables involved. It should be endowed with an implicit of type Numeric[T]
 * @param variable the variable to assign
 * @param linearForm the linear form representing the right hand side of the assignment
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

case class LinearAssignment[T](variable: Int, linearForm: LinearForm[T]) (implicit numeric: Numeric[T])  {
  import numeric._

  override def toString = "v"+ variable + " := " + linearForm.toString

  def analyze[Property <: NumericalProperty[Property]] (input: Property): Property =
    input.linearAssignment(variable, linearForm.toDouble)
}
