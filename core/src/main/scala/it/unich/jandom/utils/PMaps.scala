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

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import TLists._

/**
 * This is an implementation of parameter maps, which are wrappers of Scala maps
 * that keeps track of the keys added to the map. Every key in a parameter map should
 * be an instance of an appropriate concrete subclass of Parameter. This is an
 * example:
 * {{{
 * val p1 = Parameter[Int]
 * val p2 = Parameter[String]
 * val p3 = Parameter[Int]
 *
 * val pmap = (p1 --> 12) +: (p2 --> "foo") +: PMap.empty
 *
 * pmap(p1)  // returns 12 of type Int
 * // pmap(p3) does not compile
 *
 * def f(m: p1.type +: PNil) {
 *   m(p1)
 * }
 *
 * f(pmap)
 * }}}
 *
 */
object PMaps {

  /**
   * A `Parameter` identifies a single key which may be included in a `PMap`.
   */
  abstract class Parameter {
    /**
     * The type of the value corresponding to this parameter in a `PMap`.
     */
    type Value

    /**
     * It builds a (key, value) pair for this parameter. It is analogous with the use of `->` to
     * build standard pairs.
     * @param v the value to associate with this parameter.
     */
    def -->(v: Value) = new ParameterValue[this.type](this, v)
  }

  /**
   * A variant of the `Parameter` class endows with a default value, which is
   * returned by a `PMap` when no such a parameter is present.
   */
  abstract class ParameterWithDefault extends Parameter {
    /**
     * The default value for this parameter.
     */
    val default: Value
  }

  object Parameter {
    /**
     * It returns a new parameter of type `PVAl`.
     */
    def apply[PVal] = new Parameter { type Value = PVal }

    /**
     * It returns a new parameter with default value `v`.
     */
    def apply[PVal](v: PVal) = new ParameterWithDefault { type Value = PVal; val default = v }
  }

  /**
   * A `ParametrValue` is a pair of a parameter and a value of its corresponding type.
   */
  final class ParameterValue[P <: Parameter](val p: P, val v: P#Value) {
    /**
     * May be used to refer to the parameter component of this pair.
     */
    type Param = P
  }

  /**
   * An exception which is thrown when we try to force the access to a non-existent key of `PMap`.
   * @param p the parameter which we tried to access.
   */
  case class MissingParameterException(val p: Parameter) extends Exception

  /**
   * `PMap` is the class for parameter maps. It is implemented delegating the store of information
   * to an actual untyped `Map`. However, the type `PList` contains a list of singleton types of
   * parameters which are ensured to be in the delegate. Other parameter may be present in the
   * map in addition to those certified by `PList`.
   */
  sealed class PMap(val delegate: Map[Parameter, Any]) {
    outer =>

    /**
     * The list of parameters ensured to be present in `delegate`.
     */
    type PList <: TList

    /**
     * Returns the value associated to parameter `p`. Parameter `p` should be present
     * in the map. The returns type of `apply` is the type of values associated to `p`.
     */
    @implicitNotFound("The parameter map has no such element")
    def apply(p: Parameter)(implicit ev: Contains[p.type, PList]): p.Value = delegate(p).asInstanceOf[p.Value]

    /**
     * Returns the value associated to parameter `p` with default values. If `p` is not present
     * in the map, the default is returned. The returns type of `apply` is the type of values associated to `p`.
     */
    def apply(p: ParameterWithDefault) = delegate.getOrElse(p, p.default).asInstanceOf[p.Value]

    /**
     * Returns an `Option[p.Value]`, analogously to the `get` method of a map.
     */
    def get(p: Parameter) = delegate.get(p).asInstanceOf[Option[p.Value]]

    /**
     * Returns an `Option[p.Value]`, analogously to the `get` method of a map. However, since `p`
     * is a parameter with default, the result is never `None`.
     */
    def get(p: ParameterWithDefault) = delegate.get(p).orElse(Some(p.default)).asInstanceOf[Option[p.Value]]

    /**
     * Returns the value associated to parameter `p` if it exists, it throws a `MissingParameterException`.
     * otherwise.
     */
    def getOrFail(p: Parameter) = delegate.getOrElse(p, throw MissingParameterException(p)).asInstanceOf[p.Value]

    /**
     * Check whether the parameter `p` is present in the map. If it is present, returns the map with a new
     * type which certify the precsence of the parameter, otherwise it returns with `MissingParameterException`.
     */
    def check[P <: Parameter](p: P) = if (delegate.contains(p))
      this.asInstanceOf[PMap { type PList = p.type :: outer.PList }]
    else
      throw MissingParameterException(p)

    /**
     * It adds a new (parameter, value) pair to the map.
     */
    def +:[P <: Parameter](pv: ParameterValue[P]) = new PMap(delegate + (pv.p -> pv.v)) { type PList = pv.Param :: outer.PList }
  }

  object PMap {
    /**
     * It returns an empty parameter map.
     */
    val empty: PNil = new PMap(Map.empty) { type PList = TNil }

    /**
     * This returns a view from a `PMap' which contains parameters in the set `A` to another map which contains parameters in
     * the set `B`, which is a subset of `A`.
     */
    implicit def view[A <: TList, B <: TList](implicit ev: SubSet[B, A]) = new Function1[PMap { type PList = A }, PMap { type PList = B }] {
      def apply(m: PMap { type PList = A }) = m.asInstanceOf[PMap { type PList = B }]
    }
  }

  /**
   * A shortand for the type of `PMap` with no certified parameters.
   */
  type PNil = PMap { type PList = TNil }

  /**
   * A shortand for the type of `PMap` whose certified parameters are those of `T` with the addition of `H`.
   */
  type +:[H <: Parameter, T <: PMap] = PMap { type PList = H :: T#PList }

  /**
   * An implicit conversion from a `PMap' which contains parameters in the set `A` to another map which contains parameters in
   * the set `B`, which is a subset of `A`.
   */
  implicit def conv[T <: TList](m: PMap)(implicit ev: SubSet[T, m.PList]) = m.asInstanceOf[PMap { type PList = T }]
} 
