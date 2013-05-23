/**
 * Copyright 2013 amato
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
 * @author amato
 *
 */

case class PairSharingProperty(val ps: Set[UP[Int]], val size: Int) extends ObjectProperty[PairSharingProperty] {
  def variable(n: Int) = n

  private def renameVariable(ps: Set[UP[Int]], newvar: Int, oldvar: Int) = ps map { _.replace(newvar, oldvar) }
  private def removeVariable(ps: Set[UP[Int]], v: Int) = ps filterNot { _.contains(v) }

  def starUnion(ps: Set[UP[Int]], v: Int) =
    (for ( UP(l,r) <- ps; if l == v || r == v;  first = if (l == v) r else l;
          UP(l1,r1) <- ps; if l1 == v || r1 == v;  second = if (l1 == v) r1 else l1 )
      yield UP(first, second)) ++ ps

  def evalConstant(c: Int) = PairSharingProperty(ps, size + 1)
  def evalNull = PairSharingProperty(ps, size + 1)
  def evalNew = PairSharingProperty(ps + UP(size, size), size + 1)
  def evalVariable(v: Variable) = if (ps contains UP(v, v))
    PairSharingProperty(ps ++ renameVariable(ps, size, v) + UP(size, v), size + 1)
  else
    PairSharingProperty(ps, size + 1)
  def evalLength = PairSharingProperty(ps, size + 1)
  def evalField(v: Int, f: Int) = if ( ps contains UP(v,v) )
	evalVariable(v)  // this may be improved with type information
  else
   	PairSharingProperty(Set(), size+1)

  def evalAdd = PairSharingProperty(ps, size - 1)
  def evalSub = PairSharingProperty(ps, size - 1)
  def evalMul = PairSharingProperty(ps, size - 1)
  def evalDiv = PairSharingProperty(ps, size - 1)
  def evalRem = PairSharingProperty(ps, size - 1)
  def evalShl = PairSharingProperty(ps, size - 1)
  def evalShr = PairSharingProperty(ps, size - 1)
  def evalUshr = PairSharingProperty(ps, size - 1)
  def evalBinOp = PairSharingProperty(ps, size - 1)
  def evalNeg = PairSharingProperty(ps, size - 1)

  def assignVariable(dst: Int) = PairSharingProperty(renameVariable(removeVariable(ps, dst), dst, size - 1), size - 1)
  def assignField(dst: Int, fieldNum: Int) =
    if ( ! ps.contains(UP(dst,dst)) )  // this should generate a null pointer exception
    	PairSharingProperty(Set(), size-1)
    else if ( ! ps.contains(UP(size-1, size-1)) )  // not required optimization
        PairSharingProperty(ps, size-1)
    else
        PairSharingProperty(starUnion(removeVariable(starUnion(ps + UP(dst, size-1), size-1), size-1), dst), size-1)

  def mkString(vars: IndexedSeq[String]) = Seq("locals: " + size + (ps map { case UP(l, r) => (vars(l), vars(r)) } mkString (" ", ",", "")))

  def union(that: PairSharingProperty) = {
    assert(size == that.size)
    PairSharingProperty(ps union that.ps, size max that.size)
  }

  def intersection(that: PairSharingProperty) = {
    assert(size == that.size)
    PairSharingProperty(ps union that.ps, size)
  }

  def widening(that: PairSharingProperty) = this union that

  def narrowing(that: PairSharingProperty) = this intersection that

  def isEmpty = false

  def tryCompareTo[B >: PairSharingProperty](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
    other match {
      case other: PairSharingProperty =>
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

class PairSharingDomain extends ObjectDomain {
  type Property = PairSharingProperty
  def top(numroots: Int) = {
    val pairs = (for (l <- 0 until numroots; r <- l until numroots) yield UP(l, r)).toSet
    PairSharingProperty(pairs, numroots)
  }
  def bottom(numroots: Int) = PairSharingProperty(Set(), numroots)
  def initial(numroots: Int) = bottom(numroots)
}
