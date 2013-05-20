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

package it.unich.sci.jandom.ui

import it.unich.sci.jandom.targets.NarrowingStrategy._

/**
 * The ParameterEnumeration for NarrowingStrategy.
 */
object NarrowingStrategies extends ParameterEnumeration[Value] {
	val name = "Narrowing Strategy"
	val description = "This parameter specifies when and whether to build descending sequences during the analysis."
	val values = Seq(
		ParameterValue(None, "None","No narrowing is performed"),
		ParameterValue(Separate, "Separate","Standard narrowing, where first all the ascendings steps are performed, then all descending steps."),
		ParameterValue(Restart, "Restart", "Localized narrowing with Restart strategy, as described in the paper submitted to SAS 2013."),
		ParameterValue(Continue, "Continue", "Localized narrowing with Continue strategy, as described in the paper submitted to SAS 2013.")
	    )
	val default = values(1)
}
