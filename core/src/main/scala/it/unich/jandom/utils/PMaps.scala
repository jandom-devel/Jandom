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

  abstract class Parameter {
    type Value
    def -->(v: Value) = new ParameterValue[this.type](this, v)
  }

  object Parameter {
    def apply[PVal] = new Parameter { type Value = PVal }
  }

  final class ParameterValue[P <: Parameter](val p: P, val v: P#Value) {
    type Param = P
  }

  sealed class PMap(val delegate: Map[Parameter, Any]) {
    outer =>
    type PList <: TList

    @implicitNotFound("The parameter map has no such element")
    def apply(p: Parameter)(implicit ev: Contains[p.type, PList]) = delegate(p).asInstanceOf[p.Value]
    def get(p: Parameter) = delegate.get(p).asInstanceOf[Option[p.Value]]
    def +:[P <: Parameter](pv: ParameterValue[P]) = new PMap(delegate + (pv.p -> pv.v)) { type PList = pv.Param :: outer.PList }
  }

  object PMap {
    val empty: PNil = new PMap(Map.empty) { type PList = TNil }

    implicit def view[A <: TList, B <: TList](implicit ev: SubSet[B, A]) = new Function1[PMap { type PList = A }, PMap { type PList = B }] {
      def apply(m: PMap { type PList = A }) = m.asInstanceOf[PMap { type PList = B }]
    }
  }

  type PNil = PMap { type PList = TNil }
  type +:[H <: Parameter, T <: PMap] = PMap { type PList = H :: T#PList }

  implicit def conv[T <: TList](m: PMap)(implicit ev: SubSet[T, m.PList]) = m.asInstanceOf[PMap { type PList = T }]

} 
