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

package it.unich.sci.jandom.targets.jvmsoot

import scala.collection.immutable.Stack

import it.unich.sci.jandom.domains.objects.UP
import it.unich.sci.jandom.targets.linearcondition.LinearCond

import soot._

/**
 * A domain for pair sharing analysis, as described by Secci and Spoto.
 * @author Gianluca Amato <gamato@unich.it>
 */

class SootFramePairSharingDomain(scene: Scene, classAnalysis: ClassReachableAnalysis, roots: IndexedSeq[Local]) extends SootFrameDomain {

  val localMap: Map[Local, Int] = roots.zipWithIndex.toMap

  def top(stack: Stack[Type]) = {
    val pairs = (for (l <- 0 until roots.size; r <- l until roots.size) yield UP(l, r)).toSet
    Property(pairs, stack)
  }
  def bottom(stack: Stack[Type]) = Property(Set(), stack)
  def initial = bottom(Stack())

  def apply(ps: Set[UP[Local]]) = Property(ps map { case UP(l,r) => UP(localMap(l), localMap(r)) }, Stack())

  case class Property(val ps: Set[UP[Int]], val stack: Stack[Type]) extends SootFrameProperty[Property] {

    def size = roots.size + stack.size

    def roots = SootFramePairSharingDomain.this.roots

    def classOfVar(i: Int): SootClass = {
      val tpe = if (i < roots.size) roots(i).getType() else stack(i - roots.size)
      assert(tpe.isInstanceOf[RefType])
      tpe.asInstanceOf[RefType].getSootClass()
    }

    private def renameVariable(ps: Set[UP[Int]], newvar: Int, oldvar: Int) = {
      val newclass = classOfVar(oldvar)
      ps map { _.replace(newvar, oldvar) } filter {
        case UP(l, r) if l == newvar && r == newvar => true
        case UP(`newvar`, r) => classAnalysis.mayShare(classOfVar(r), newclass)
        case UP(l, `newvar`) => classAnalysis.mayShare(classOfVar(l), newclass)
        case _ => true
      }
    }

    private def removeVariable(ps: Set[UP[Int]], v: Int) = ps filterNot { _.contains(v) }

    private def delUntrackedVariable = Property(ps, stack.pop)
    private def addUntrackedVariable(tpe: Type) = Property(ps, stack.push(tpe))

    def starUnion(ps: Set[UP[Int]], v: Int) =
      (for (
        UP(l, r) <- ps; if l == v || r == v; first = if (l == v) r else l;
        UP(l1, r1) <- ps; if l1 == v || r1 == v; second = if (l1 == v) r1 else l1
      ) yield UP(first, second)) ++ ps

    def evalConstant(c: Int) = addUntrackedVariable(IntType.v())
    def evalNull = addUntrackedVariable(IntType.v())
    def evalNew(tpe: Type) = Property(ps + UP(size, size), stack.push(tpe))
    def evalLocal(l: Local) = {
      val v = localMap(l)
      val newstack = stack.push(l.getType())
      if (ps contains UP(v, v))
        Property(ps ++ renameVariable(ps, size, v) + UP(size, v), newstack)
      else
        Property(ps, newstack)
    }
    def evalLength = addUntrackedVariable(IntType.v())
    def evalField(l: Local, f: SootField) = {
      val v = localMap(l)
      if (ps contains UP(v, v))
        Property(ps ++ renameVariable(ps, size, v) + UP(size, v), stack.push(f.getType()))
      else
        Property(ps, stack.push(f.getType()))
    }
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

    def evalLinearForm(lf: Array[Double]) = addUntrackedVariable(DoubleType.v())
    def testLinearCondition(lc: LinearCond) = (this, this)

    def assignLocal(l: Local) = {
      val dst = localMap(l)
      if (l.getType().isInstanceOf[RefType])
        Property(renameVariable(removeVariable(ps, dst), dst, size - 1), stack.pop)
      else
        Property(ps, stack.pop)
    }

    def assignField(l: Local, f: SootField) = {
      val dst = localMap(l)
      if (!ps.contains(UP(dst, dst))) // this should generate a null pointer exception
        Property(Set(), stack.pop)
      else if (!ps.contains(UP(size - 1, size - 1))) // not required optimization
        Property(ps, stack.pop)
      else
        Property(starUnion(removeVariable(starUnion(ps + UP(dst, size - 1), size - 1), size - 1), dst), stack.pop)
    }

    def mkString(vars: IndexedSeq[String]) =
      (ps.view.toSeq map { case UP(l, r) => s"(${vars(l)}, ${vars(r)})" }) :+ s"dimension ${size}"

    def union(that: Property) = {
      assert(stack == that.stack)
      Property(ps union that.ps, stack)
    }

    def intersection(that: Property) = {
      assert(stack == that.stack)
      Property(ps union that.ps, stack)
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
