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

import scala.language.implicitConversions

import it.unich.jandom.domains.AbstractDomain
import it.unich.scalafix.Box

/**
 * This object contains the WideningSpec class and its subclasses. They are used
 * to specify which kind of widening to use for the analyses.
 * @author Gianluca Amato <gianluca.amato@unich.it>
 */
object WideningSpecs {

  /**
   * An  object of the WideningSpec class is a specification for the widening to use in the
   * analysis. Each object has a get method which, once provided with an abstract domain,
   * provides the correct widening in the form of a box.
   */
  sealed abstract class WideningSpec {
    /**
     * Returns the box corresponding to the specification of this object.
     */
    def get(dom: AbstractDomain): Box[dom.Property]
  }

  /**
   * This specifies the default widening of an abstract domain.
   */
  case object DefaultWidening extends WideningSpec {
    def get(dom: AbstractDomain) = dom.defaultWidening
  }

  /**
   * This specifies to use upper bound as a widening. Therefore,
   * this is not formally a real narrowing since it may lead to non-terminating
   * computations.
   */
  case object UpperBoundWidening extends WideningSpec {
    def get(dom: AbstractDomain) = Box.upperBound[dom.Property](dom.ScalaFixDomain)
  }

  /**
   * This specifies to delay the application of the `base` widening for `d` steps.
   */
  case class DelayedWidening(base: WideningSpec, d: Int) extends WideningSpec {
    def get(dom: AbstractDomain) = if (d > 0) Box.cascade(UpperBoundWidening.get(dom), d, base.get(dom)) else base.get(dom)
  }

  /**
   * This specified a widening given its name
   */
  case class NamedWidening(name: String) extends WideningSpec {
    def get(dom: AbstractDomain) = dom.widening(name)
  }

  implicit def stringToNameWidening(name: String): WideningSpec = NamedWidening(name)
}
