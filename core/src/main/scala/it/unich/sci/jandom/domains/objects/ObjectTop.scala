/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

package it.unich.sci.jandom.domains.objects

/**
 * This is the class for object properties of the top domain. There is a single `ObjectTop`
 */
case class ObjectTop() extends ObjectProperty[ObjectTop] {
  def tryCompareTo[B >: ObjectTop](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: ObjectTop => Some(0)
    case _ => None
  }
  def widening(that: ObjectTop) = this
  def union(that: ObjectTop) = this
  def narrowing(that: ObjectTop) = this
  def intersection(that: ObjectTop) = this
  def mkString(vars: IndexedSeq[String]): Seq[String] = Seq("top")
}

/**
 * The most abstract object domain.
 * @author Gianluca Amato <gamato@unich.it>
 */
object ObjectTopDomain extends ObjectDomain {
  type Property = ObjectTop
  def full(n: Int) = new Property()
  def empty(n: Int) = new Property()
}
