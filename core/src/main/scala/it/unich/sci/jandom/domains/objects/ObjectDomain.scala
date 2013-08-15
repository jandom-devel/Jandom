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

import it.unich.sci.jandom.domains.DimensionFiberedDomain
import it.unich.sci.jandom.domains.DimensionFiberedProperty

/**
 * This trait represents a domain which handles objects and their relationship. May be used, for
 * example, for sharing analysis. This is only a draft, and will be probably improved along the
 * development of Jandom.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait ObjectDomain extends DimensionFiberedDomain {

  type Property <: ObjectProperty[Property]

  /**
   * This trait represents single abstract element in an object domain.
   */
  trait ObjectProperty[Property <: ObjectProperty[Property]] extends DimensionFiberedProperty[Property] {
    this: Property =>

    type ShareFilter = UP[Int] => Boolean

    def addFreshVariable: Property
    def removeRangeOfVariables(range: Range): Property
    def removeLowerVariables(newSize: Int): Property
    def removeHigherVariables(newSize: Int): Property
    def assignNull(dst: Int = dimension - 1): Property
    def assignVariable(dst: Int, src: Int): Property
    def assignVariableToField(dst: Int, field: Int, src: Int): Property
    def assignFieldToVariable(dst: Int, src: Int, field: Int, mayShare: ShareFilter = (_ => true)): Property
    def filter(mayShare: ShareFilter): Property
    def isNull(v: Int): Boolean
    def testNull(v: Int): Property
    def testNotNull(v: Int): Property
    def connect(that: Property, common: Int): Property
  }
}
