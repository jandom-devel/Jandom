/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.objectmodels

/**
 * This is an object model used in several tests. It has 5 types (tsuper, tmiddle, tsub, tother, tprim),
 * subject to the following relationship: tsuper < tmiddle < tsub, tother < tsuper. Moreover, tprim
 * is a root primitive type.
 * @author Gianluca Amato <gamato@unich.it>
 */

object TestObjectModel extends TreeObjectModel with NoArrays with ObjectModelHelper {
  type Type = AnyRef
  type Field = Char

  val tmiddle = "tmiddle"
  val tsuper = "tsuper"
  val tsub = "tsub"
  val tother = "tother"
  val tprim = "tprim"

  def parents(t: Type) = t match {
    case `tsuper` => Set()
    case `tmiddle` => Set(tsuper)
    case `tsub` => Set(tmiddle)
    case `tother` => Set(tsuper)
    case `tprim` => Set()
  }

  def children(t: Type) = t match {
    case `tsuper` => Set(tother, tmiddle)
    case `tmiddle` => Set(tsub)
    case `tsub` => Set()
    case `tother` => Set()
    case `tprim` => Set()
  }

  def lteq(t1: Type, t2: Type) =
    t1 == t2 || (t1 == tsub && t2 != tother && t2 != tprim) || (t2 == tsuper && t1 != tprim)

  def declaredFields(t: Type) = t match {
    case `tsuper` => Set('a','b')
    case `tmiddle` => Set('c')
    case `tsub` => Set('d')
    case `tother` => Set('e')
    case `tprim` => Set()
  }

  def typeOf(f: Field) = f match {
    case 'a' => tsuper
    case 'b' => tsuper
    case 'c' => tmiddle
    case 'd' => tsub
    case 'e' => tmiddle
  }

  def isPrimitive(t: Type) = (t == tprim)

  def isConcrete(t: Type) = true
}
