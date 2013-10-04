/* Copyright 2013 Gianluca Amato
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

package it.unich.sci.jandom.ui

import it.unich.sci.jandom.domains.numerical.BoxDoubleDomain
import it.unich.sci.jandom.domains.numerical.NumericalDomain
import it.unich.sci.jandom.domains.numerical.ParallelotopeDomain
import scala.collection.mutable.Buffer
import scala.util.Try

/**
 * The ParameterEnumeration for numerical domains.
 */
object NumericalDomains extends ParameterEnumeration[NumericalDomain] {
  val name = "Domain"
  val description = "The numerical domain to use for the analysis."

  val values: Buffer[ParameterValue[NumericalDomain]] = Buffer(
    ParameterValue(BoxDoubleDomain(), "BoxDouble", "This is a native Scala implementation of boxes. It is safe " +
      "w.r.t. double arithmetics."),
    ParameterValue(BoxDoubleDomain(overReals=true), "BoxDouble over Reals", "This is a native Scala implementation of boxes. It is safe " +
      "w.r.t. reals."),
    ParameterValue(ParallelotopeDomain(), "Parallelotope", "This is a native Scala implementation of parallelotopes. It is " +
      "not safe and should not be used."))
  val default = values.head

  // Load objects PPLUIInitializer and PPLMacroUIInitializer if available
  Try ( Class.forName("it.unich.sci.jandom.ui.PPLUIInitializer$") )
  Try ( Class.forName("it.unich.sci.jandom.ui.PPLMacroUIInitializer$") )
}
