/**
  * Copyright 2014, 2018 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.targets

import it.unich.jandom.domains.numerical.NumericalProperty

/**
  * This is a target class for the numeric assignments "v := exp".
  *
  * @param v   the variable we want to assign a value to
  * @param exp the expression we want to assign
  * @author Gianluca Amato <gamato@unich.it>
  */
case class NumericAssignment(v: Int, exp: NumericExpression) {
  /**
    * This methods takes an input property and returns the result of computing the assignment.
    */
  def analyze[Property <: NumericalProperty[Property]](input: Property): Property = exp.assignTo(v)(input)

  /**
    * Returns the string representation of the assignment using the given list of
    * vaiable names.
    */
  def mkString(vars: Seq[String]) = s"${vars(v)} := ${exp.mkString(vars)}"

  /**
    * Returns the dimension of the assignment, i.e. the greatest variable index which
    * occurs in the expression plus one.
    */
  def dimension: Int = exp.dimension max (v + 1)

  override def toString = s"v$v := $exp"
}
