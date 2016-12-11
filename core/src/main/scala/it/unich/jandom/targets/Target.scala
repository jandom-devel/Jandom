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

import scala.collection.mutable.HashMap

import it.unich.jandom.domains.AbstractDomain
import it.unich.scalafix.EquationSystem

/**
 * The abstract class for targets, which are the static analyzers for the
 * different target languages.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
abstract class Target[Tgt <: Target[Tgt]] {
  /**
   * Abstract type for program points.
   */
  type ProgramPoint

  /**
   * Returns the exit program point in the target, if it exists.
   */
  val lastPP: Option[ProgramPoint]

  /**
   * Target cannot work for all static analyzers, hence we specify here the base domain in the hierarchy
   * which is supported by the target.
   */
  type DomainBase <: AbstractDomain

  /**
   * An alias for parameters in input by the analyzer.
   */
  protected type Parameters = it.unich.jandom.targets.Parameters[Tgt]

  /**
   * Returns an empty annotation which is well suited for this target. The
   * default implementation just returns an HashMap.
   */
  def getAnnotation[Property]: Annotation[ProgramPoint,Property] =
    new HashMap[ProgramPoint,Property] with Annotation[ProgramPoint,Property]

  /**
   * Perform a static analysis over the target.
   * @param param the parameters which drive the analyzer
   * @return an annotation for the program
   */
  def analyze(params: Parameters): Annotation[ProgramPoint,params.Property]

  /**
   * Return an Equation System corresponding to the target
   */
  def toEQS(dom: DomainBase): EquationSystem[ProgramPoint, dom.Property] = ???
}
