/**
 * Copyright 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.targets.parameters

import it.unich.jandom.domains.AbstractDomain
import it.unich.scalafix.Box

/**
 * This object contains the NarrowingSpec class and its subclasses. They are used
 * to specify which kind of narrowing to use for the analyses.
 * @author Gianluca Amato <gianluca.amato@unich.it>
 */
object NarrowingSpecs {

  /**
   * An  object of the NarrowingSpec class is a specification for the narrowing to use in th
   * analysis. Each object has a get method which, once provided with an abstract domain,
   * provides the correct narrowing in the form of a box.
   */
  sealed abstract class NarrowingSpec {
    /**
     * Returns the box corresponding to the specification of this object.
     */
    def get(dom: AbstractDomain): Box[dom.Property]
  }

  /**
   * This specifies the default narrowing of an abstract domain.
   */
  case object DefaultNarrowing extends NarrowingSpec {
    def get(dom: AbstractDomain) = Box { (a: dom.Property, b: dom.Property) => a narrowing b }
  }

  /**
   * This specifies the trivial narrowing which never improves results.
   */
  case object TrivialNarrowing extends NarrowingSpec {
    def get(dom: AbstractDomain) = Box.left[dom.Property]
  }

  /**
   * This always returns its right argument. Therefore, this is not formally a
   * real widening since it may lead to non-terminating computations.
   */
  case object LowerBoundNarrowing extends NarrowingSpec {
    def get(dom: AbstractDomain) = Box.right[dom.Property]
  }

  /**
   * This specifies to delay the application of the `base` widening for `d` steps.
   */
  case class DelayedNarrowing(base: NarrowingSpec, d: Int) extends NarrowingSpec {
    def get(dom: AbstractDomain) = base.get(dom).delayed(d)
  }
}
