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

package it.unich.jandom.ppfactories

import it.unich.jandom.narrowings.DelayedNarrowing
import it.unich.jandom.narrowings.Narrowing

/**
 * The factory for a delayed narrowing. It creates a number of delayed narrowings with the same parameters.
 * @tparam ProgramPoint the type of program point
 * @param narrowingFactory the original narrowing factory
 * @param delay the delay of the narrowing
 * @author Gianluca Amato <gamato@unich.it>
 */
class DelayedNarrowingFactory[ProgramPoint] (private val narrowingFactory: PPFactory[ProgramPoint,Narrowing], private val delay: Int) extends PPFactory[ProgramPoint,Narrowing] {
  require(delay>=0)
  def apply(pp: ProgramPoint) = new DelayedNarrowing(narrowingFactory(pp), delay)  
}

/**
 * The companion object for delayed narrowing factories
 **/
object DelayedNarrowingFactory {
  def apply[ProgramPoint](narrowingFactory: PPFactory[ProgramPoint,Narrowing], delay: Int) = new DelayedNarrowingFactory(narrowingFactory,delay)
}
