/**
  * Copyright 2013, 2018 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.domains

/**
  * This class represents a single element in a DimensionFiberedDomain.
  *
  * @author Gianluca Amato <gamato@unich.it>
  */
trait DimensionFiberedProperty[Property <: DimensionFiberedProperty[Property]] extends CartesianFiberedProperty[Unit, Property] {
  this: Property =>

  type Domain <: DimensionFiberedDomain

  /**
    * Add a new variable.
    */
  def addVariable(): Property

  /**
    * Add `m` new variables.
    *
    * @note `m` should be positive
    */
  def addVariables(m: Int): Property = {
    require(m >= 0)
    (0 until m).foldLeft(this)((p, _) => p.addVariable())
  }

  def fiber: Seq[Unit] = Seq.fill[Unit](dimension)(())

  def addVariable(t: Unit): Property = addVariable()
}
