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
import it.unich.jandom.targets.parameters.NarrowingSpecs._
import it.unich.jandom.targets.parameters.WideningSpecs._
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
   * This is the domain to use for the analysis. It needs to be compatible with the target type.
   */
  val domain: Tgt#DomainBase

  /**
   * The property to analyze.
   */
  type Property = domain.Property

  // we cannot initialize here to `domain.defaultWidening` since domain is initialized later
  private var _widening: BoxAssignment[Tgt#ProgramPoint, domain.Property] = null

  /**
   * The current widening returned as a box assignment.
   */
  def widening: BoxAssignment[Tgt#ProgramPoint, domain.Property] = {
    if (_widening == null)
      _widening = domain.defaultWidening
    _widening
  }

  /**
   * Set the widening starting from a box assignment.
   */
  def widening_=(box: BoxAssignment[Tgt#ProgramPoint, domain.Property]): Unit = {
    _widening = box
  }

  /**
   * Set the widening starting from a widening specification.
   */
  def widening_=(wspec: WideningSpec): Unit = {
    _widening = wspec.get(domain)
  }

  // we cannot initialize here to `_narrowing.get(domain)` since domain is initialized later
  private var _narrowing: BoxAssignment[Tgt#ProgramPoint, domain.Property] = null

  /**
   * The current narrowing, returned as a box assignment.
   */
  def narrowing: BoxAssignment[Tgt#ProgramPoint, domain.Property] = {
    if (_narrowing == null) _narrowing = DefaultNarrowing.get(domain)
    _narrowing
  }

  /**
   * Set the narrowing starting from a box assignment.
   */
  def narrowing_=(box: BoxAssignment[Tgt#ProgramPoint, domain.Property]): Unit = {
    _narrowing = box
  }

  /**
   * Set the widening starting from a narrowing specification.
   */
  def narrowing_=(nspec: NarrowingSpec): Unit = {
    _narrowing = nspec.get(domain)
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
    override def write(cbuf: Array[Char], off: Int, len: Int) = {}
    override def flush() = {}
    override def close() = {}
    override def toString = ""
  }

  def log(msg: String): Unit = {
    debugWriter.write(" " * nestingLevel * 3)
    debugWriter.write(msg)
  }
}
