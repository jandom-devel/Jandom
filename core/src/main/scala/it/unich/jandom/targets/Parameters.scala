/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.targets

import it.unich.jandom.targets.parameters._
import it.unich.jandom.targets.parameters.Narrowings._
import it.unich.jandom.targets.parameters.Widenings._
import it.unich.jandom.ui.NarrowingStrategies
import it.unich.jandom.ui.WideningScopes
import it.unich.scalafix.BoxAssignment

/**
 * This class is used to provide parameters for analyzers. Each instance of `Parameters` is
 * connected to a specific target and domain. Other parameters may be changed freely.
 * @tparam Tgt the type of the target
 * @author Gianluca Amato <gianluca.amato@unich.it>
 *
 */
abstract class Parameters[Tgt <: Target[Tgt]] {
  /**
   * This is the domain to use for the analysis. It need to be compatible with the target type.
   */
  val domain: Tgt#DomainBase

  /**
   * The property to analyze.
   */
  type Property = domain.Property

  private var _widening: Widening = DefaultWidening

  // we cannot initialize here to `_widening.get(domain)` since domain is initialized later
  private var _wideningAssignment: BoxAssignment[Tgt#ProgramPoint, domain.Property] = null

  /**
   * The widening specification used in the analysis. Defaults to the standard widening.
   */
  def widening: Widening = _widening

  def widening_=(w: Widening) = {
    _widening = w
    _wideningAssignment = widening.get(domain)
  }

  /**
   * The widening assignment corresponding to the current widening specification.
   */
  def wideningAssignment = {
    if (_wideningAssignment == null) _wideningAssignment = _widening.get(domain)
    _wideningAssignment
  }

  private var _narrowing: Narrowing = DefaultNarrowing

  // we cannot initialize here to `_narrowing.get(domain)` since domain is initialized later
  private var _narrowingAssignment: BoxAssignment[Tgt#ProgramPoint, domain.Property] = null

  /**
   * The narrowing used in the analysis. Defaults to the standard narrowing.
   */
  def narrowing: Narrowing = _narrowing

  def narrowing_=(n: Narrowing) = {
    _narrowing = n
    _narrowingAssignment = narrowing.get(domain)
  }

  /**
   * The narrowing assignment corresponding to the current narrowing specification.
   */
  def narrowingAssignment = {
    if (_narrowingAssignment == null) _narrowingAssignment = _narrowing.get(domain)
    _narrowingAssignment
  }

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
   * This parameter specify the strategy used to compute data-flow equations.
   */
  var iterationStrategy = IterationStrategy.Worklist

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
}
