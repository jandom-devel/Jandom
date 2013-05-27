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

import it.unich.sci.jandom.targets.linearcondition.LinearCond
import soot._

/**
 * A domain for pair sharing analysis, as described by Secci and Spoto.
 * @author Gianluca Amato <gamato@unich.it>
 */

class PairSharingDomain(scene: Scene, roots: IndexedSeq[Local]) extends ObjectDomain {

  val localMap: Map[Local, Int] = roots.zipWithIndex.toMap

  def top(stacksize: Int = 0) = {
    val pairs = (for (l <- 0 until roots.size; r <- l until roots.size) yield UP(l, r)).toSet
    Property(pairs, roots.size + stacksize)
  }
  def bottom(stacksize: Int = 0) = Property(Set(), roots.size + stacksize)
  def initial = bottom(0)

  case class Property(val ps: Set[UP[Int]], val size: Int) extends ObjectProperty[Property] {

    def roots = PairSharingDomain.this.roots

    private def renameVariable(ps: Set[UP[Int]], newvar: Int, oldvar: Int) = ps map { _.replace(newvar, oldvar) }
    private def removeVariable(ps: Set[UP[Int]], v: Int) = ps filterNot { _.contains(v) }

    private def delUntrackedVariable = Property(ps, size - 1)
    private def addUntrackedVariable = Property(ps, size + 1)

    def starUnion(ps: Set[UP[Int]], v: Int) =
      (for (
        UP(l, r) <- ps; if l == v || r == v; first = if (l == v) r else l;
        UP(l1, r1) <- ps; if l1 == v || r1 == v; second = if (l1 == v) r1 else l1
      ) yield UP(first, second)) ++ ps

    def evalConstant(c: Int) = addUntrackedVariable
    def evalNull = Property(ps, size + 1)
    def evalNew = Property(ps + UP(size, size), size + 1)
    def evalLocal(l: Local) = {
      val v = localMap(l)
      if (ps contains UP(v, v))
        Property(ps ++ renameVariable(ps, size, v) + UP(size, v), size + 1)
      else
        Property(ps, size + 1)
    }
    def evalLength = addUntrackedVariable
    def evalField(l: Local, f: SootField) = evalLocal(l) // may be improved with type information

    def evalAdd = delUntrackedVariable
    def evalSub = delUntrackedVariable
    def evalMul = delUntrackedVariable
    def evalDiv = delUntrackedVariable
    def evalRem = delUntrackedVariable
    def evalShl = delUntrackedVariable
    def evalShr = delUntrackedVariable
    def evalUshr = delUntrackedVariable
    def evalBinOp = delUntrackedVariable
    def evalNeg = delUntrackedVariable

    def evalGt = delUntrackedVariable
    def evalGe = delUntrackedVariable
    def evalLt = delUntrackedVariable
    def evalLe = delUntrackedVariable
    def evalEq = delUntrackedVariable
    def evalNe = delUntrackedVariable

    def test = {
      val dropped = delUntrackedVariable
      (dropped, dropped)
    }

    def testGt = evalGt.test
    def testGe = evalGe.test
    def testLe = evalLe.test
    def testLt = evalLt.test
    def testEq = evalEq.test
    def testNe = evalNe.test

    def evalLinearForm(lf: Array[Double]) = addUntrackedVariable
    def testLinearCondition(lc: LinearCond) = (this, this)

    def assignLocal(l: Local) = {
      val dst = localMap(l)
      Property(renameVariable(removeVariable(ps, dst), dst, size - 1), size - 1)
    }
    def assignField(l: Local, f: SootField) = {
      val dst = localMap(l)
      if (!ps.contains(UP(dst, dst))) // this should generate a null pointer exception
        Property(Set(), size - 1)
      else if (!ps.contains(UP(size - 1, size - 1))) // not required optimization
        Property(ps, size - 1)
      else
        Property(starUnion(removeVariable(starUnion(ps + UP(dst, size - 1), size - 1), size - 1), dst), size - 1)
    }

    def mkString(vars: IndexedSeq[String]) =  (ps.view.toSeq map { case UP(l, r) => s"(${vars(l)}, ${vars(r)})" }) :+ s"dimension ${size}"

    def union(that: Property) = {
      assert(size == that.size)
      Property(ps union that.ps, size max that.size)
    }

    def intersection(that: Property) = {
      assert(size == that.size)
      Property(ps union that.ps, size)
    }

    def widening(that: Property) = this union that

    def narrowing(that: Property) = this intersection that

    def isEmpty = false

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
