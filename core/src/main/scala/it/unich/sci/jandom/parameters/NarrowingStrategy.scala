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

package it.unich.sci.jandom.parameters

/**
 * This parameter specifies when and whether to build descending sequences during the analysis. 
 * The available alternatives are:
 * - None: no decending steps are performed
 * - Separate: first all the ascendings steps are performed, then all descending steps
 * - Restart: the standard Random strategy of perfoming Narrowing intertwined with Widening
 * - Continue: similar to Restart, but during narrowing of outer loops, inner loops only performs narrowing
 * At the moment, this is only supported by the SLIL target.
 */
object NarrowingStrategy extends ParameterEnumeration {   
	type NarrowingStrategy = Value
	val name = "Narrowing Strategy"
	val shortName = "narrowing"
	val description = "This parameter specifies when and whether to build descending sequences during the analysis."
	val None = Value("None","No narrowing is performed")
	val Separate = Value("Separate","Narrowing is performed at the end, after the ascending phase is concluded")
	val Restart = Value("Restart", "Narrowing in intertwined with ascending phase")
	val Continue = Value("Continue", "Similar to Restart, but after a node is descending, it never ascends again")
	val default = Separate 
}
