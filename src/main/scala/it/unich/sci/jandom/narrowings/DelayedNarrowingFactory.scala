/**
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it ll be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 *
 * (c) 2012 Gianluca Amato
 */

package it.unich.sci.jandom
package narrowings

import targets.Target
import ppfactories.PPFactory

/**
 * The factory for a delayed narrowing. It creates a number of delayed narrowings with the same parameters.
 * @tparam Tgt the target for the narrowing factory
 * @param narrowingFactory the original narrowing factory
 * @param delay the delay of the narrowing
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class DelayedNarrowingFactory[Tgt<: Target] (private val narrowingFactory: PPFactory[Tgt,Narrowing], private val delay: Int) extends PPFactory[Tgt,Narrowing] {
  require(delay>=0)
  def apply(pp: Tgt#WideningPoint) = new DelayedNarrowing(narrowingFactory(pp), delay)  
}

/**
 * The companion object for delayed narrowing factories
 **/
object DelayedNarrowingFactory {
  def apply[Tgt <:Target](narrowingFactory: PPFactory[Tgt,Narrowing], delay: Int) = new DelayedNarrowingFactory(narrowingFactory,delay)
}
