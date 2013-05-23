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
 * Class for unordered pairs. It is essentially a set with one or two elements, but
 * we do no implement that interface yet.
 * @param A the type of the elements in the pair.
 */
class UP[@specialized(Int) A](x: A, y:A)(implicit ordering: Ordering[A]) {
  val first = ordering.min(x,y)
  val second = ordering.max(x,y)

  def replace(newval: A, oldval: A) =
	 UP( if (first == oldval) newval else first, if (second == oldval) newval else second )
  def contains(v: A) = first == v || second == v

  override def equals(that: Any) = that match {
    case that: UP[A] => first == that.first && second == that.second
    case _ => false
  }
  override def hashCode = first.hashCode * 41 + second.hashCode
  override def toString = (first,second).toString
}

object UP {
  def apply[A: Ordering](x: A, y: A) = new UP(x,y)
  def apply[A: Ordering](p: (A,A)) = new UP(p._1, p._2)
  def unapply[A](up: UP[A]): Option[(A,A)] = Some((up.first,up.second))
}
