/**
  * Copyright 2013, 2018 Gianluca Amato
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

import it.unich.jandom.domains.AbstractDomain

import scala.collection.mutable

/**
  * The abstract class for targets, which are the static analyzers for the
  * different target languages.
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  *
  */
abstract class Target[Tgt <: Target[Tgt]] {
  /**
    * Abstract type for program points.
    */
  type ProgramPoint

  /**
    * The exit program point in the target, if it exists.
    */
  val lastPP: Option[ProgramPoint]

  /**
    * Target might not with work all abstract domain, hence we specify here the base domain
    * in the hierarchy which is supported by the target.
    */
  type DomainBase <: AbstractDomain

  /**
    * An alias for parameters in input by the analyzer.
    */
  type Parameters = it.unich.jandom.targets.Parameters[Tgt]

  /**
    * Returns an empty annotation which is well suited for this target. The
    * default implementation just returns a mutable HashMap.
    */
  def getAnnotation[Property]: Annotation[ProgramPoint, Property] =
    new mutable.HashMap[ProgramPoint, Property] with Annotation[ProgramPoint, Property]

  /**
    * Perform a static analysis over the target.
    *
    * @param params the parameters which drive the analyzer
    * @return an annotation for the program
    */
  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property]

  /**
    * A TargetAdapter contains the result of transforming the current target to a
    * destination target. It contains both the resulting target object, and some
    * methods to interpret the result of the analysis on the destination target
    * in terms of the result of the analysis in the current target.
    *
    * @tparam TargetDst the type of the resulting target.
    */
  abstract class TargetAdapter[TargetDst <: Target[TargetDst]] {
    /**
      * This is the resulting target.
      */
    val transformed: TargetDst

    /**
      * This method converts an annotation for the `transformed` targed into an annotation for the
      * current target.
      */
    def pullbackAnnotation[V](ann: Annotation[transformed.ProgramPoint, V]): Annotation[ProgramPoint, V]
  }

}
