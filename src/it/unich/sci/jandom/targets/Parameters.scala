/**
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
 *
 * (c) 2012 Gianluca Amato
 */
package it.unich.sci.jandom
package targets
  
import domains.NumericalProperty
import domains.NumericalDomain
import widenings.{WideningFactory, DefaultWideningFactory}
import narrowings.Narrowing
import narrowings.DefaultNarrowing
/**
 * This class is used to keep parameter for analyzers.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class Parameters[Property <: NumericalProperty[Property], Tgt <: Target] (val domain: NumericalDomain[Property], val tgt: Tgt) {  
  /**
  * The standard widening for the domain
  */
  var wideningFactory: WideningFactory = DefaultWideningFactory
  
  /**
   * The standard narrowing for the domain
   */
  var narrowing: Narrowing[Property] = new DefaultNarrowing[Property]
}
