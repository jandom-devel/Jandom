/**
 * Copyright 2014, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

/**
 * This parameter specifies when and whether to build descending sequences during the analysis. 
 * The available alternatives are:
 * - None: no decending steps are performed
 * - Separate: standard narrowing, where first all the ascendings steps are performed, then all descending steps
 * - Restart: localized narrowing with Restart strategy, as described in the paper submitted to SAS 2013
 * - Continue: localized narrowing with Continue strategy,  as described in the paper submitted to SAS 2013
 * At the moment, this is only supported by the LTS target.
 */
object NarrowingStrategy extends Enumeration {   
	type NarrowingStrategy = Value	
	val None = Value
	val Separate = Value
	val Restart = Value
	val Continue = Value	 
}
