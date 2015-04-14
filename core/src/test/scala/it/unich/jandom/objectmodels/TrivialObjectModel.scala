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
 * This is a trivial object model with a single non-primitive type, no fields and no arrays.
 * @author Gianluca Amato <gamato@unich.it>
 */
object TrivialObjectModel extends TreeObjectModel with NoArrays with ObjectModelHelper {
  self: ObjectModel =>
  type Type = Unit
  type Field = Unit
  def declaredFields(t: Type) = Set()
  def typeOf(f: Field) = {}
  def lteq(t1: Type, t2: Type) = true
  def parents(t: Type) = Set()
  def children(t: Type) = Set()
  def isPrimitive(t: Type) = false
  def isConcrete(t: Type) = true
  override def concreteApprox(t1: Type, t2: Type) = Some(())
  override def concreteApprox(ts: Iterable[Type]) = if (ts.isEmpty) None else Some(())
  override def mayShare(t1: Type, t2: Type) = true
  override def mayBeAliases(t1: Type, t2: Type) = true
}
