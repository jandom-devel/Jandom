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

import scala.collection.immutable.Range

/**
 * This is the implementation of the untypd part of PairSharing as in Spoto and Secci.
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
object PairSharingDomain extends ObjectDomain {

  def top(dimension: Int) = allPairs(0 until dimension, dimension)
  def bottom(dimension: Int) = new Property(Set(), dimension)
  def apply(ps: Set[UP[Int]], dimension: Int) = new Property(ps, dimension)

  def allPairs(vars: Set[Int], dimension: Int) =
    apply(for (i <- vars; j <- vars) yield UP(i, j), dimension)

  def allPairs(vars: Seq[Int], size: Int) = {
    val pairs = for (i <- 0 until vars.size; j <- i until vars.size) yield UP(vars(i), vars(j))
    apply(pairs.toSet, size)
  }

  case class Property(val ps: Set[UP[Int]], val dimension: Int) extends ObjectProperty[Property] {

    type Domain = PairSharingDomain.this.type

    def domain = PairSharingDomain.this

    private def renameVariable(ps: Set[UP[Int]], newvar: Int, oldvar: Int) = ps map { _.replace(newvar, oldvar) }
    private def removeVariable(ps: Set[UP[Int]], v: Int) = ps filterNot { _.contains(v) }
    private def starUnion(ps: Set[UP[Int]], v: Int) =
      (for (
        UP(l, r) <- ps; if l == v || r == v; first = if (l == v) r else l;
        UP(l1, r1) <- ps; if l1 == v || r1 == v; second = if (l1 == v) r1 else l1
      ) yield UP(first, second)) ++ ps

    def addVariable() = {
      val ps2 = for (UP(i, j) <- ps; if (i == j)) yield UP(i, dimension)
      new Property(ps ++ ps2, dimension + 1)
    }

    def delVariable(n: Int) =
      if (n == dimension - 1)
        new Property(removeVariable(ps, n), dimension - 1)
      else
        new Property(removeVariable(ps, n) map { _.replace { x => if (x < n) x else x - 1 } }, dimension-1)

    def mapVariables(rho: Seq[Int]) = {
        val ps2 = for ( UP(l, r) <- ps; if rho(l) != -1; if rho(r) != -1) yield UP(rho(l),rho(r))
        new Property(ps2, dimension - rho.count { _ == -1 })
    }

    def addFreshVariable = new Property(ps + UP((dimension, dimension)), dimension + 1)

    /**
     * This is similar to connect, but do not remove the common properties.
     */
    def connectFull(that: Property, common: Int) = {
      assert(common <= dimension && common <= that.dimension)
      // index of the first common variable in the connected property
      val firstCommonInThis = dimension - common
      // remove all pairs in that involving a variable which is null in this. At the
      // same time, translate index
      val trimmedTranslatedThat = for {
        pair @ UP(l, r) <- that.ps
        if l >= common || !isNull(l + firstCommonInThis)
        if r >= common || !isNull(r + firstCommonInThis)
      } yield UP(l + firstCommonInThis, r + firstCommonInThis)
      // remove from this those pairs which only relates to common variables
      val trimmedThis = this.ps filter { case UP(l, r) => l < firstCommonInThis }
      // join one ps of this with one ps of that
      val j1 = for {
        UP(l, r) <- trimmedThis
        if r >= firstCommonInThis
        UP(l1, r1) <- trimmedTranslatedThat
        if r == l1
      } yield UP(l, r1)
      // join two ps of this
      val j2 = for (UP(l, r) <- trimmedThis; if r >= firstCommonInThis; UP(l1, r1) <- j1; if r == r1) yield UP(l, l1)
      Property(trimmedThis ++ j1 ++ j2 ++ trimmedTranslatedThat, dimension - common + that.dimension)
    }

    def connect(that: Property, common: Int) = {
      connectFull(that, common).delVariables(dimension - common until dimension)
    }

    def assignNull(dst: Int) = new Property(removeVariable(ps, dst), dimension)

    def assignVariable(dst: Int, src: Int) = {
      val removed = removeVariable(ps, dst)
      if (isNull(src))
        new Property(removed, dimension)
      else
        new Property(removed ++ renameVariable(removed, dst, src) + UP(dst, src), dimension)
    }

    def assignFieldToVariable(dst: Int, src: Int, field: Int, mayShare: ShareFilter) = {
      val removed = removeVariable(ps, dst)
      if (isNull(src))
        new Property(removed, dimension)
      else {
        val renamed = renameVariable(removed, dst, src) filter mayShare
        new Property(removed ++ renamed + UP(dst, src), dimension)
      }
    }

    def assignVariableToField(dst: Int, field: Int, src: Int) =
      if (isNull(dst))
        bottom
      else
        new Property(starUnion(starUnion(ps + UP(dst, src), src), dst), dimension)

    def filter(mayShare: ShareFilter) = new Property(ps filter mayShare, dimension)

    def isNull(v: Int) = !(ps contains UP(v, v))

    def testNull(v: Int) = new Property(removeVariable(ps, v), dimension)

    def testNotNull(v: Int) = if (isNull(v)) bottom else this

    def isTop = ((0 until dimension) zip (0 until dimension)) forall { case (i, j) => ps contains UP(i, j) }

    def isBottom = ps.isEmpty

    def isEmpty = false

    def top = PairSharingDomain.top(dimension)

    def bottom = PairSharingDomain.bottom(dimension)

    def union(that: Property) = {
      assert(dimension == that.dimension)
      new Property(ps union that.ps, dimension)
    }
    def intersection(that: Property) = {
      assert(dimension == that.dimension)
      new Property(ps intersect that.ps, dimension)
    }

    def widening(that: Property) = union(that)

    def narrowing(that: Property) = narrowing(that)

    def mkString(vars: Seq[String]) = {
      val pairs = ps.toSeq map { case UP(l, r) => s"(${vars(l)}, ${vars(r)})" }
      s"[ ${pairs.mkString(", ")} ] dimension ${dimension}"
    }

    override def toString = mkString(for (i <- 0 until dimension) yield i.toString).mkString(" ")

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
      other match {
        case other: Property =>
          if (dimension == other.dimension) {
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
