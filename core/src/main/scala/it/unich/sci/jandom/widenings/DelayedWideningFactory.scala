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

package it.unich.sci.jandom.widenings

import it.unich.sci.jandom.targets.Target
import it.unich.sci.jandom.ppfactories.PPFactory

/**
 * The factory for a delayed widening. It creates a number of delayed widenings with the same parameters.
 * @tparam ProgramPoint the type of program point
 * @param wideningFactory the original widening factory
 * @param delay the delay of the widening
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class DelayedWideningFactory[ProgramPoint] (private val wideningFactory: PPFactory[ProgramPoint,Widening], private val delay: Int) extends PPFactory[ProgramPoint,Widening] {
  require(delay>=0)
  def apply(pp: ProgramPoint) = new DelayedWidening(wideningFactory(pp), delay)  
}

/**
 * The companion object for delayed widening factories
 **/
object DelayedWideningFactory {
  def apply[ProgramPoint](wideningFactory: PPFactory[ProgramPoint,Widening], delay: Int) = new DelayedWideningFactory(wideningFactory,delay)
}
