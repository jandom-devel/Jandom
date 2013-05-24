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

import it.unich.sci.jandom.targets.linearcondition.LinearCond

/**
 * @author amato
 *
 */

case class PairSharingProperty(val ps: Set[UP[Int]], val size: Int) extends ObjectProperty[PairSharingProperty] {
  def variable(n: Int) = n

  private def renameVariable(ps: Set[UP[Int]], newvar: Int, oldvar: Int) = ps map { _.replace(newvar, oldvar) }
  private def removeVariable(ps: Set[UP[Int]], v: Int) = ps filterNot { _.contains(v) }

  def delVariable = PairSharingProperty(ps, size-1)
  def addVariable(tpe: soot.Type) = PairSharingProperty(ps, size+1)

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

  def evalAdd = delVariable
  def evalSub = delVariable
  def evalMul = delVariable
  def evalDiv = delVariable
  def evalRem = delVariable
  def evalShl = delVariable
  def evalShr = delVariable
  def evalUshr = delVariable
  def evalBinOp = delVariable
  def evalNeg = delVariable

  def evalGt = delVariable
  def evalGe = delVariable
  def evalLt = delVariable
  def evalLe = delVariable
  def evalEq = delVariable
  def evalNe = delVariable

  def evalLinearForm(lf: Array[Double]) = PairSharingProperty(ps, size+1)

  def test = {
    val dropped = delVariable
    (dropped, dropped)
  }

  def testGt = evalGt.test
  def testGe = evalGe.test
  def testLe = evalLe.test
  def testLt = evalLt.test
  def testEq = evalEq.test
  def testNe = evalNe.test
  def testLinearCondition(lc: LinearCond) = ( this, this )


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
  def top(roots: Seq[soot.Type]) = {
    val pairs = (for (l <- 0 until roots.size; r <- l until roots.size) yield UP(l, r)).toSet
    PairSharingProperty(pairs, roots.size)
  }
  def bottom(roots: Seq[soot.Type]) = PairSharingProperty(Set(), roots.size)
  def initial(roots: Seq[soot.Type]) = bottom(roots)
}
