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

package it.unich.jandom.targets

import it.unich.jandom.narrowings.DefaultNarrowing
import it.unich.jandom.narrowings.Narrowing
import it.unich.jandom.ppfactories.PPFactory
import it.unich.jandom.widenings.DefaultWidening
import it.unich.jandom.widenings.Widening
import it.unich.jandom.ui.WideningScopes
import it.unich.jandom.ui.NarrowingStrategies
import it.unich.jandom.ppfactories.DelayedWideningFactory
import it.unich.jandom.ppfactories.DelayedNarrowingFactory
import it.unich.jandom.narrowings.NoNarrowing

/**
 * This class is used to provide parameters for analyzers. Each instance of `Parameters` is
 * connected to a specific target and domain. Other parameters may be changed freely.
 * @tparam Tgt the type of the target
 * @param tgt the target for the analysis
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
abstract class Parameters[Tgt <: Target[Tgt]]  {
  /**
   * This is the domain to use for the analysis. It need to be compatible with the target type.
   */
  val domain: Tgt#DomainBase

  /**
   * The property to analyze.
   */
  type Property = domain.Property

  /**
   * The widening factory used in the analysis. Defaults to the factory for the standard domain widening.
   */
  var wideningFactory: PPFactory[Tgt#ProgramPoint, Widening] = DefaultWidening

  /**
   * The narrowing factory used in the analysis. Defaults to the standard domain narrowing.
   */
  var narrowingFactory: PPFactory[Tgt#ProgramPoint, Narrowing] = DefaultNarrowing

  /**
   * This parameter determines whether results are saved for each program point or only for widening points.
   */
  var allPPResult = true

  /**
   * This parameter should be defined for inter-procedural analsysis.
   */
  var interpretation: Option[Interpretation[Tgt, this.type]] = None

  /**
   * This parameter determines whether standard or local widening is used. At the moment, this is only supported
   * by the SLSL target.
   */
  var wideningScope = WideningScope.Output
  
  /**
   * This parameter determines where to put widenings.
   */
  var wideningLocation = WideningNarrowingLocation.Loop
  
  /**
   * This parameter determines where to put narrowings.
   */
  var narrowingLocation = WideningNarrowingLocation.Loop
  
  /**
   * This parameter determine the interlacing strategy between narrowing and widening
   */
  var narrowingStrategy = NarrowingStrategy.Restart

  /**
   * If it is true, computes an io semantic
   */
  var io = false

  /**
   * This is used for putting results in tags
   */
  var tag = scala.collection.mutable.Map[Any, Property]()

  /**
   * This is a variable globally used by the analyzer for keeping track of nested level
   */
  var nestingLevel = 0

  /**
   * This is a java writer where the analyzer write debug informations
   */
  var debugWriter = new java.io.Writer {
    override def write(cbuf: Array[Char], off: Int, len: Int) {}
    override def flush() {}
    override def close() {}
    override def toString = ""
  }

  def log(msg: String) {
    debugWriter.write(" " * nestingLevel * 3)
    debugWriter.write(msg)
  }
  
  def setParameters[T <: Target[T]](wideningIndex:Int, narrowingIindex:Int, delay:Int, debug:Boolean) {
    wideningScope = WideningScopes.values(wideningIndex).value
    narrowingStrategy = NarrowingStrategies.values(narrowingIindex).value
    if (delay != 0) {
      wideningFactory = DelayedWideningFactory(DefaultWidening, delay)
    }
    narrowingFactory = DelayedNarrowingFactory(NoNarrowing, 2)
    if (debug) debugWriter = new java.io.StringWriter
  }
}
