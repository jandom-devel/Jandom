/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.utils

/**
 * An `IterableFunction` is a partial function which may be iterated to get a collection
 * of all its bindings. It is hal way between a `PartialFunction` and a `Map`.
 * @tparam U the domain of the function.
 * @tapram T the codomain of the function.
 */
trait IterableFunction[U, +T] extends PartialFunction[U, T] with Iterable[(U, T)] {
  /**
   * Returns the domain of the function. 
   */
  def keys: Iterable[U]
  
  /**
   * Returns the domain of the function as a Set 
   */
  def keySet: collection.Set[U]  
  
  /**
   * Prints a sequence of all the bindings.
   */
  override def toString = (for ((u, v) <- iterator) yield s"${u} -> ${v}").mkString("[", " ,", "]")
}

object IterableFunction {
  /**
   * Returns an empty iterable function.
   */
  def empty[U, T] = new IterableFunction[U, T] {
    def apply(x: U) = throw new NoSuchElementException("argument not found: " + x)
    def isDefinedAt(x: U) = false
    def iterator = Iterator.empty
    def keys = Iterable.empty
    def keySet = Set.empty
  }

  /**
   * An implicit class which builds an iterable function from a map.
   */
  implicit class mapAssignment[U, T](m: collection.Map[U, T]) extends IterableFunction[U, T] {
    def apply(x: U) = m(x)
    def isDefinedAt(x: U) = m.isDefinedAt(x)
    def iterator = m.iterator
    def keys = m.keys
    def keySet = m.keySet
  }
}
