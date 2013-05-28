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
 * This is the implementation of the untypd part of PairSharing as in Spoto and Secci.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
object PairSharingDomain extends ObjectDomain {

  def top(size: Int) = allPairs(0 until size, size)
  def bottom(size: Int) = new Property(Set(), size)
  def apply(ps: Set[UP[Int]], size: Int) = new Property(ps, size)

  def allPairs(vars: Set[Int], size: Int) =
    apply(for (i <- vars; j <- vars) yield UP(i, j), size)

  def allPairs(vars: Seq[Int], size: Int) = {
    val pairs = for (i <- 0 until vars.size; j <- i until vars.size) yield UP(vars(i), vars(j))
    apply(pairs.toSet, size)
  }

  case class Property(val ps: Set[UP[Int]], val size: Int) extends ObjectProperty[Property] {

    private def renameVariable(ps: Set[UP[Int]], newvar: Int, oldvar: Int) = ps map { _.replace(newvar, oldvar) }
    private def removeVariable(ps: Set[UP[Int]], v: Int) = ps filterNot { _.contains(v) }
    private def starUnion(ps: Set[UP[Int]], v: Int) =
      (for (
        UP(l, r) <- ps; if l == v || r == v; first = if (l == v) r else l;
        UP(l1, r1) <- ps; if l1 == v || r1 == v; second = if (l1 == v) r1 else l1
      ) yield UP(first, second)) ++ ps

    def addVariable = new Property(ps + UP((size, size)), size + 1)
    def delVariable(n: Int) =
      if (n == size - 1)
        new Property(removeVariable(ps, n), size - 1)
      else
        new Property(removeVariable(renameVariable(renameVariable(ps, size, n), n, size - 1), size), size - 1)

    def assignNull(dst: Int) = new Property(removeVariable(ps, dst), size)

    def assignVariable(dst: Int, src: Int) = {
      val removed = removeVariable(ps, dst)
      if (isNull(src))
        new Property(removed, size)
      else
        new Property(removed ++ renameVariable(removed, dst, src) + UP(dst, src), size)
    }

    def assignVariabelAndDelete(dst: Int, src: Int) =
      new Property(renameVariable(ps, dst, src), size).delVariable(src)

    // without type information we cannot be more precise
    def assignFieldToVariable(dst: Int, src: Int, field: Int) =
      assignVariable(dst, src)

    def assignVariableToField(dst: Int, field: Int, src: Int) =
      if (isNull(dst))
        bottom(size)
      else
        new Property(starUnion(starUnion(ps + UP(dst, src), src), dst), size)

    def isNull(v: Int) = !(ps contains UP(v, v))

    def isTop = false
    def isBottom = false
    def isEmpty = false

    def union(that: Property) = {
      assert(size == that.size)
      new Property(ps union that.ps, size)
    }
    def intersection(that: Property) = {
      assert(size == that.size)
      new Property(ps intersect that.ps, size)
    }

    def widening(that: Property) = union(that)

    def narrowing(that: Property) = narrowing(that)

    def mkString(vars: IndexedSeq[String]) =
      (ps.view.toSeq map { case UP(l, r) => s"(${vars(l)}, ${vars(r)})" }) :+ s"dimension ${size}"

    override def toString = mkString(for (i <- 0 until size) yield i.toString).mkString(" ")

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
      other match {
        case other: Property =>
          if (size == other.size) {
            if (other.ps == ps)
              Some(0)
            else if (ps subsetOf other.ps)
              Some(-1)
            else if (other.ps subsetOf ps)
              Some(1)
            else
              None
          } else
            None
        case _ => None
      }
  }
}
