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

package it.unich.sci.jandom.ppfactories

import scala.collection.mutable.Map
import it.unich.sci.jandom.targets.Target
import it.unich.sci.jandom.targets.Annotation

/**
 * A "per program point factory" which reuses objects for the same program point.
 * @tparam ProgramPoint the type of program point
 * @param factory the nested factory for building new objects
 * @param ann an annotation used to memoize the objects
 * @author Gianluca Amato <amato@sci.unich.it>
 */

class MemoizingFactory[ProgramPoint, T](private val factory: PPFactory[ProgramPoint, T],
  private val ann: Annotation[ProgramPoint, T]) extends PPFactory[ProgramPoint, T] {

  def apply(pp: ProgramPoint) = {
    ann.get(pp) match {
      case Some(v) => v
      case None => { val v = factory(pp); ann(pp) = v; v }
    }
  }
}

/**
 * The companion object for per program points factories
 */
object MemoizingFactory {
  /**
   * Builds a "per program point" factory, given an annotation
   * @tparam ProgramPoint the type of program point
   * @tparam T the type of the returned value for the factory
   * @param factory the factory to build new widenings when needed
   * @param ann annotation used to stored the generated widenings
   * @return the factory
   */
  def apply[ProgramPoint, T](factory: PPFactory[ProgramPoint, T], ann: Annotation[ProgramPoint, T]) =
    new MemoizingFactory(factory, ann)

  /**
   * Builds  a "per program point" factory using the standard annotation for a given target
   * @tparam Tgt the type of target for the factory
   * @tparam T the type of the returned value for the factory
   * @param factory the factory to build new widenings when needed
   * @param tgt the target
   * @return the factory
   */
  def apply[Tgt <: Target, T](tgt: Tgt)(factory: PPFactory[tgt.ProgramPoint, T]): PPFactory[tgt.ProgramPoint,T] =
    new MemoizingFactory(factory, tgt.getAnnotation[T])
}
