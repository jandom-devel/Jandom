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
package ppfactories

import targets.Target
import annotations._
import scala.collection.mutable.Map

/**
 * A "per program point factory" which reuses objects for the same program point.
 * @tparam Tgt the type of target for the factory
 * @param factory the nested factory for building new objects
 * @param ann an annotation used to memoize the objects 
 * @author Gianluca Amato <amato@sci.unich.it>
 */

class MemoizingFactory[Tgt <: Target,T] (private val factory: PPFactory[Tgt,T], 
                                          private val ann: Tgt#Annotation[T]) extends PPFactory[Tgt,T] {
  
  def apply(pp: Tgt#WideningPoint) = {
    // this is not typesafe: I am converting a dependent type into an independent type. But,
    // otherwise, each factory should contain a target
    val ann2 = ann.asInstanceOf[scala.collection.mutable.Map[Tgt#WideningPoint, T]]
    ann2.get(pp) match {
      case Some(v) => v
      case None => { val v= factory(pp); ann2(pp)=v; v }
    }
  }  
} 

/**
 * The companion object for per program points factories
 */
object MemoizingFactory {
  /**
   * Builds a "per program point" factory, given a PerProgramPointAnnotation
   * @tparam Tgt the type of target for the factory
   * @tparam T the type of the returned value for the factory
   * @param factory the factory to build new widenings when needed
   * @param ann a per-pp annotation used to stored the generated widenings
   * @return the factory
   */
  def apply[Tgt <: Target, T](factory: PPFactory[Tgt,T], ann: Tgt#Annotation[T]) = 
    new MemoizingFactory(factory, ann)
  
  /**
   * Builds  a "per program point" factory using the standard PerProgramPointAnnotation for a given target
   * @tparam Tgt the type of target for the factory
   * @tparam T the type of the returned value for the factory
   * @param factory the factory to build new widenings when needed  
   * @param tgt the target 
   * @param annBuilder the program point annotation builder for the given target
   * @return the factory
   */
  def apply[Tgt <: Target, T](factory: PPFactory[Tgt,T], tgt: Tgt) = 
    new MemoizingFactory(factory, tgt.getAnnotation[T])
}
