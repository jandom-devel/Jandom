/**
  * Copyright 2018 Filippo Sestini, Tobia Tesan
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

package it.unich.jandom.domains.numerical.octagon
import it.unich.jandom.utils.dbm._
import it.unich.jandom.utils.numberext.IField
import math.PartiallyOrdered


/**
  * The definition of an object exposing a subset of the transfer
  * functions and attributes detailed in Mine' 2006, "The Octagon
  * Abstract Domain".
  *
  * Specifically, we expose the functionality that has an intersection
  * Jandom, so for example backward assignment and the like has been
  * removed.
  *
  * Most notably, assignments of the form Vj0 <- ... + [a,b] have been
  * replaced with Vj0 <- ... + [c,c] == c
  */
trait Octagon[N <: IField[N]] extends PartiallyOrdered[Octagon[N]] {
  // Abstract tests (Mine' 2006 fig. 20 p. 42)
  def test_vj0_plus_c_le_0 (j0 : Var, c : N) : Octagon[N]
  def test_minus_vj0_plus_c_le_0 (j0 : Var, c : N) : Octagon[N]
  def test_vj0_minus_vi0_plus_c_le_0(vj0 : Var, vi0 : Var, c : N) : Octagon[N]
  def test_vj0_plus_vi0_le_c(vj0 : Var, vi0 : Var, c : N) : Octagon[N]
  def test_minus_vj0_minus_vi0_plus_c_le_0(vj0 : Var, vi0 : Var, c : N) : Octagon[N]

  // Abstract assignment (Mine' 2006 fig. 15 p. 35)
  def forget(f : Var) : Octagon[N]
  ///
  def assign_vj0_gets_c(j0 : Var, c : N) : Octagon[N]
  def assign_vj0_gets_vj0_plus_c(j0 : Var, c : N) : Octagon[N]
  def assign_vj0_gets_vi0_plus_c(j0 : Var, i0 : Var, c : N) : Octagon[N]
  def assign_vj0_gets_minus_vj0(j0 : Var) : Octagon[N]
  def assign_vj0_gets_minus_vi0(j0: Var, i0 : Var) : Octagon[N] =
    this.assign_vj0_gets_vi0(j0, i0).assign_vj0_gets_minus_vj0(j0)//
  def assign_vj0_gets_minus_vj0_plus_c (j0 : Var, c : N) : Octagon[N] =
    this.assign_vj0_gets_minus_vj0(j0).assign_vj0_gets_vj0_plus_c(j0, c) //
  def assign_vj0_gets_minus_vi0_plus_c (j0 : Var, i0 : Var, c : N) : Octagon[N] =
    this.assign_vj0_gets_minus_vi0(j0, i0).assign_vj0_gets_vj0_plus_c(j0, c) //

  // This one is a little extra wrt the cases on the paper, it's
  // needed for assign_vj0_gets_minus_vi0 Just implement it as a call
  // to vj0_gets_vi0_plus_c with c = zero, when you have a zero for
  // your N
  protected def assign_vj0_gets_vi0(j0 : Var, i0 : Var) : Octagon[N]

  //////
  def isEmpty : Boolean
  def isBottom : Boolean
  def union(that : Octagon[N]) : Octagon[N]
  def intersection(that : Octagon[N]) : Octagon[N]
  def isTop : Boolean
  def dimension : OctagonDim
  //////
  def widening(that : Octagon[N]) : Octagon[N]
  def narrowing(that : Octagon[N]) : Octagon[N]

  //////

  /**
    * Note: being a PatriallyOrdered means that you can compare
    * octagons with <=.
    *
    * == calls .equals, which unless defined in each implementation
    * amounts by default to reference comparison; you may want to use
    * <= & >= instead.
    */
  def tryCompareTo[B >: Octagon[N]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int]

  def get_ineq_vi_minus_vj_leq_c(i : SignedVarIdx, j : SignedVarIdx) : Option[N]

}

case class BottomOctagon[N <: IField[N]] (override val dimension : OctagonDim) extends Octagon[N] {
  def test_vj0_plus_c_le_0 (j0 : Var, c : N) : Octagon[N] = this
  def test_minus_vj0_plus_c_le_0 (j0 : Var, c : N) : Octagon[N] = this
  def test_vj0_minus_vi0_plus_c_le_0(vj0 : Var, vi0 : Var, c : N) : Octagon[N] = this
  def test_vj0_plus_vi0_le_c(vj0 : Var, vi0 : Var, c : N) : Octagon[N] = this
  def test_minus_vj0_minus_vi0_plus_c_le_0(vj0 : Var, vi0 : Var, c : N) : Octagon[N] = this


  def get_ineq_vi_minus_vj_leq_c(i : SignedVarIdx, j : SignedVarIdx) : Option[N] = None

  //////
  def forget(f : Var) : Octagon[N] = this
  def assign_vj0_gets_c(j0 : Var, c : N):  Octagon[N] = this
  def assign_vj0_gets_vj0_plus_c(j0 : Var, c : N) : Octagon[N] = this
  def assign_vj0_gets_vi0_plus_c(j0 : Var, i0 : Var, c : N) : Octagon[N] = this
  def assign_vk_gets_e(k : Int) : Octagon[N] = this
  def assign_vj0_gets_minus_vj0(j0 : Var) : Octagon[N] = this
  def assign_vj0_gets_vi0 (j0 : Var, i0 : Var) : Octagon[N] = this
  //////
  def isEmpty : Boolean = true
  def isBottom : Boolean = true
  def isTop : Boolean = false
  def union(that : Octagon[N]) = that
  def intersection(that : Octagon[N]) = this
  def widening(that : Octagon[N]) : Octagon[N] = that
  def narrowing(that : Octagon[N]) : Octagon[N] = ???
  //////
  def tryCompareTo[B >: Octagon[N]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
    other match {
      case that : Octagon[N] =>
        if (that.isBottom) Some(0)
        else Some(-1)
      case _ => None
    }
}

//////////////////////
// UTILS
//////////////////////

trait SignedVarIdx {
  def i : Int
  def bar : SignedVarIdx
  def coeff : Int
  def toVar : Var
}

/*
 *  Each variable Vi ∈ V has both a positive form V_2i−1, and a
 *  negative form V2i in V. Mine' 2006 p. 8.
 */
object SignedVarIdx {
  def apply(i : Int) : SignedVarIdx = if (i % 2 == 0)
    VMinusIdx(i)
  else
    VPlusIdx(i)
}

case class Var(i : Int) {
  require(i != 0)
  def negForm : VMinusIdx = VMinusIdx(2 * i)
  def posForm : VPlusIdx = VPlusIdx(2 * i - 1)
}

case class VPlusIdx(i : Int) extends SignedVarIdx {
  require (i != 0)
  require(i % 2 == 1)
  require (i != 0)
  def bar = VMinusIdx(i + 1)
  def coeff : Int = +1
  def toVar = Var((i + 1) / 2)
}

case class VMinusIdx(i : Int) extends SignedVarIdx {
  require (i != 0)
  require(i % 2 == 0)
  def bar = VPlusIdx(i - 1)
  def coeff : Int = -1
  def toVar = Var(i / 2)
}

case class OctagonDim(private val n : Int) {
  require(n != 0)
  def toDBMDim : DBMDim = DBMDim(n * 2)
  def octagonDimToInt : Int = n
  def allVars : Seq[Var] = (1 to n).map(Var(_))
}

object OctagonDim {
  def apply(d : DBMDim) : OctagonDim = OctagonDim(d.dbmDimToInt / 2)
}
