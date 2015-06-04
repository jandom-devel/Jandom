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

package it.unich.jandom.fixpoint

/**
 * This is the trait for a factory which builds objects of type T for any element of type Unknown.
 * It is essentially a map Unknown => T, but it has a special name so that specific implicits may be
 * defined for it.
 * @tparam Unknown the type of unknowns
 * @tapram T the type of values returned by the factory
 * @author Gianluca Amato <gamato@unich.it>
 */
abstract class UFactory[-Unknown, +T] extends Function1[Unknown, T]

object UFactory {
  /**
   * An implicit factory which always returns the same value.
   * @tparam T the type of the object built by the factory
   * @param obj the object returned by the factory
   * @author Gianluca Amato <gamato@unich.it>
   */
  implicit class constantUFactory[T](obj: T) extends UFactory[Any, T] {
    def apply(x: Any) = obj
  }

  /**
   * An implicit factory which builds an UFactory from a function.
   * @tparam Unknown the type of unknowns
   * @tapram T the type of values returned by the factory
   * @author Gianluca Amato <gamato@unich.it>
   */
  implicit class functionUFactory[Unknown, T](f: Unknown => T) extends UFactory[Unknown, T] {
    def apply(x: Unknown) = f(x)
  }

  /**
   * Builds a factory according to the predicate `p`. For each unknown `u`, if `p` is satisfied for `u`, the ifTrue factory is called,
   * otherwise the ifFalse is called.
   */
  def choose[Unknown, T](s: Unknown => Boolean, ifTrue: UFactory[Unknown, T], ifFalse: UFactory[Unknown, T]) = new UFactory[Unknown, T] {
    def apply(x: Unknown) = if (s(x)) ifTrue(x) else ifFalse(x)
  }
  
  /**
   * Builds a factory which delegates to another factory but memoize the result.
   */
  def memoize[Unknown, T](delegate: UFactory[Unknown, T]) = new UFactory[Unknown, T] {
    val memo = collection.mutable.Map.empty[Unknown, T]
    def apply(x: Unknown) = memo.getOrElseUpdate(x, delegate(x))
  }

  /**
   * A factory which builds box assignments using the cascade method of boxes. It should be used for delayed widening, because
   * each program point should provide a different widening, which should be memoized in order to be effective. 
   */
  def cascade[Unknown, T](first: UFactory[Unknown, Box[T]], delay: Int, second: UFactory[Unknown, Box[T]]) = memoize(new UFactory[Unknown, Box[T]] {
    def apply(x: Unknown) = Box.cascade(first(x), delay, second(x))
  })
}
