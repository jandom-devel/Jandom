/** Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.ui

import scala.collection.mutable.Buffer
import scala.util.Try
import it.unich.jandom.domains.numerical._

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
    ParameterValue(ParallelotopeRationalDomain(), "Parallelotope over Rationals", "This is a native Scala implementation of parallelotopes using rational numbers."),
    ParameterValue(SumBoxDoubleParallelotopeRationDomain(), "BoxDouble + ParallelotopeRational", "Sum of boxes and parallelotopes."),
    ParameterValue(OctagonDomain(), "Octagon Domain", "This is a native Scala implementation of octagons. Currentyl, it does not support the Soot analysis.")
  )
  val default = values.head

  // Load objects PPLUIInitializer and PPLMacroUIInitializer if available
  Try ( Class.forName ("it.unich.jandom.ui.PPLUIInitializer$") )
  Try ( Class.forName ("it.unich.jandom.ui.PPLMacroUIInitializer$") )
  Try ( Class.forName ("it.unich.jandom.ui.ApronUIInitializer$") )
}
