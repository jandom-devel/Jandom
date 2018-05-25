/**
  * Copyright 2018 Tobia Tesan
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

package it.unich.jandom.domains.numerical.octagon.simple
import it.unich.jandom.domains.numerical.octagon._
import it.unich.jandom.utils.dbm._
import it.unich.jandom.utils.numberext.IField
import it.unich.jandom.utils.numberext.StaticIField

import scala.language.implicitConversions

/**
  * A simple implementation of an octagonal representation that uses a
  * DBM as its backend (basically, this _is_ a Mine'-style octagon,
  * the artificial separation is for convenience of e.g. testing,
  * stubbing).
  *
  * This is horrendously inefficient in that it maintains the
  * invariant that the inner DBM is _always_ closed.
  *
  * On the other hand, it is easy to read, verify and compare with the
  * paper, and serves as a ground truth for testing the more
  * complicated implementation under optimized._, which, as the name
  * suggests, tries to delay closure and opportunistically closes the
  * matrix when needed.
  *
  * There are *two* ways in which the optimized implementation addresses this:
  *
  * 1. Keeps a non-closed inner matrix and performs the closure on request
  *
  * 2. Performs an incremental or noop closure when there is proof of
  *    its sufficiency.
  *
  * THERE IS QUITE A LOT OF DUPLICATION between this and optimized._,
  * this is on purpose, as adding the necessary levels of indirection
  * through composition and inheritance would pollute this class to
  * the point of defying its purpose as the simplest possible
  * implementation.
  */
class SimpleOctagon[N <: IField[N]]
  (val m : ClosedDBM[N])
  (implicit ifield : StaticIField[N], cs : MineFloydWarshall[N])
    extends Octagon[N] {

  //////////////////////////////////////////////////////////////////
  // Some sugar to make the following stuff read more like Mine 2006
  //////////////////////////////////////////////////////////////////

  val ∞ = ifield.PositiveInfinity
  val _0 = ifield.zero

  implicit def OctagonDimToDBMDim(d: OctagonDim) : DBMDim = d.toDBMDim
  implicit def octftodbmf (f : OctIdx => N) : DBMIdx => N = (idx : DBMIdx) => f(OctIdx(idx))
  implicit def octidxtodbmidx (idx : OctIdx) : DBMIdx = idx.idx
  implicit def DBMtoOctagonDim(d : DBMDim) : OctagonDim = {
    require(d.dbmDimToInt > 0 && (d.dbmDimToInt) % 2 == 0)
    OctagonDim(d.dbmDimToInt / 2)
  }
  case class OctIdx(idx : DBMIdx) {
    def i = SignedVarIdx(idx.i)
    def j = SignedVarIdx(idx.j)
  }

  //////////////////////////////
  // End sugar
  //////////////////////////////


  //////////////////////////////////////////////////////////////////
  // Some utilities
  //////////////////////////////////////////////////////////////////


  protected def wrap(dbm : Option[ClosedDBM[N]]) : Octagon[N]  = dbm match {
    case Some(dbm) => new SimpleOctagon[N](dbm)
    case None => BottomOctagon[N](this.dimension)
  }

  def stronglyClosed (d : DBM[N]) = wrap(cs.strongClosure(d))
  def stronglyClosed (n : DBMDim)(f : DBMIdx => N) = wrap(cs.stronglyClosed(n)(f))

  // If the argument is closed the result is guaranteed to ble closed already (Mine' 2006 Fig. 26 p. 54)
  def forget(f : Var) = stronglyClosed(m.dimension)(forget_f(m)(f))

  def forget_f(m : DBM[N]) (f : Var) (idx : OctIdx) =
      if (
        idx.i != f.posForm &
          idx.i != f.negForm &
          idx.j != f.posForm &
          idx.j != f.negForm)
        m(idx) : N
      else if (idx.i == idx.j & idx.j == f.posForm | idx.i == idx.j & idx.j == f.negForm)
        _0 : N
      else
        ∞ : N

  ////////////////////////////////////
  // Begin octagon transfer functions
  ////////////////////////////////////

  /////////////////////////////
  // Begin abstract tests
  /////////////////////////////

  def test_vj0_plus_c_le_0 (j0 : Var, c : N) : Octagon[N] =
    // Case 1. Mine' 2006 fig. 20 p. 42
    // {{ Vj0 + c <= 0 ? }}
    stronglyClosed(m.dimension)(
      (idx : OctIdx) =>
      if (idx.i == j0.negForm & idx.j == j0.posForm)
        m(idx).min(-c._x_2)
      else
        m(idx))

  def test_minus_vj0_plus_c_le_0 (j0 : Var, c : N) : Octagon[N] =
    // Case 2. Mine' 2006 fig. 20 p. 42
    // {{ -Vj0 + c <= 0 ? }}
    // Note: this can be improved upon as
    // 1. the argument of this op does not need to be closed;
    // 2. the result can be closed via Inc*
    // Same for the following ops
    stronglyClosed(m.dimension)(
      (idx : OctIdx) =>
      if (idx.i == j0.posForm & idx.j == j0.negForm)
        m(idx).min(-c._x_2)
      else
        m(idx))

  def test_vj0_minus_vi0_plus_c_le_0(j0 : Var, i0 : Var, c : N) : Octagon[N] = {
    // Case 3. Mine' 2006 fig. 20 p. 42
    // {{ Vj0 - Vi0 + c <= 0 ? }}
    require(j0 != i0) // see fig. 20
    stronglyClosed(m.dimension)(
        (idx : OctIdx) =>
        if (idx.i == i0.posForm & idx.j == j0.posForm
          | idx.i == j0.negForm & idx.j == i0.negForm
        )
          m(idx).min(-c)
        else
          m(idx))}

  def test_vj0_plus_vi0_le_c(j0 : Var, i0 : Var, c : N) : Octagon[N] = {
    // Case 4. Mine' 2006 fig. 20 p. 42
    // {{ Vj0 + Vi0- + c <= 0 ? }}
    require(j0 != i0) // see fig. 20
    stronglyClosed(m.dimension)(
        (idx : OctIdx) =>
        if (idx.i == i0.negForm & idx.j == j0.posForm
          | idx.i == j0.negForm & idx.j == i0.posForm
        )
          m(idx).min(-c)
        else
          m(idx))}

  def test_minus_vj0_minus_vi0_plus_c_le_0(j0 : Var, i0 : Var, c : N) : Octagon[N] = {
    // Case 5. Mine' 2006 fig. 20 p. 42
    // {{ -Vj0 + Vi0- + c <= 0 ? }}
    require(j0 != i0)
    stronglyClosed(m.dimension)(
      (idx : OctIdx) =>
      if (idx.i == i0.posForm & idx.j == j0.negForm
        | idx.i == j0.posForm & idx.j == i0.negForm
      )
        m(idx).min(-c)
      else
        m(idx))}

  ///////////////////////
  // End abstract tests
  ///////////////////////


  /////////////////////////////
  // Begin abstract assignments
  /////////////////////////////

  def assign_vj0_gets_vi0(j0 : Var, i0 : Var) : Octagon[N] = assign_vj0_gets_vi0_plus_c(j0, i0, _0)

  def assign_vj0_gets_c(j0 : Var, c : N) : Octagon[N] = {
    // Case 1. Mine' 2006 fig. 15 p. 35
    // {{ Vj0 <- c }}
    stronglyClosed(m.dimension)(
      (idx : OctIdx) => {
        if (idx.i == j0.posForm & idx.j == j0.negForm)
          (-c._x_2)
        else if (idx.i == j0.negForm & idx.j == j0.posForm)
          (c._x_2)
        else
          forget_f(m)(j0)(idx)
      })}

  def assign_vj0_gets_vj0_plus_c(j0 : Var, c : N) : Octagon[N] =
    // Case 2. Mine' 2006 fig. 15 p. 35
    // {{ Vj0 <- Vj0 + c }}
    stronglyClosed(m.dimension)(
      // Assignments Vj0 ← Vj0 + [a, b] and Vj0 ← −Vj0 + [a,
      // b] do not require strongly closed matrix arguments but
      // preserve the strong closure (Mine' 2006 p. 34)
      (idx : OctIdx) =>
      if (idx.i == j0.posForm & idx.j != j0.posForm & idx.j != j0.negForm ||
        idx.j == j0.negForm & idx.i != j0.posForm & idx.i != j0.negForm)
        m(idx) - c // Jandom doesn't support interval assignment, so [a,b] = [c,c]
      else if
        (idx.i != j0.posForm  & idx.i != j0.negForm & idx.j == j0.posForm ||
          idx.j != j0.posForm  & idx.j != j0.negForm & idx.i == j0.negForm)
        m(idx) + c
      else if (idx.i == j0.posForm & idx.j == j0.negForm)
        m(idx) - (c._x_2)
      else if (idx.i == j0.negForm & idx.j == j0.posForm)
        m(idx) + (c._x_2)
      else
        m(idx))


  def assign_vj0_gets_vi0_plus_c(j0 : Var, i0 : Var, c : N) : Octagon[N] = {
    // Case 3. Mine' 2006 fig. 15 p. 35
    // {{ Vj0 <- Vi0 + c }}
    require (j0 != i0)
    stronglyClosed(m.dimension)(
      (idx : OctIdx) =>
      if (idx.i == j0.posForm & idx.j == i0.posForm
        | idx.i == i0.negForm  & idx.j == j0.negForm)
        -c
      else
        if (idx.i == i0.posForm & idx.j == j0.posForm
          | idx.i == j0.negForm  & idx.j == i0.negForm)
        c
      else
        forget_f(m)(j0)(idx))}

  def assign_vj0_gets_minus_vj0(j0 : Var) : Octagon[N] = {
    // Case 4. Mine' 2006 fig. 15 p. 35: {{ Vj0 <- -Vj0 }
    stronglyClosed(m.dimension)(
      // Assignments Vj0 ← Vj0 + [a, b] and Vj0 ← −Vj0 + [a,
      // b] do not require strongly closed matrix arguments but
      // preserve the strong closure (Mine' 2006 p. 34)
      (idx : OctIdx) =>
      if ((Seq(j0.posForm, j0.negForm) contains idx.i) &
        !(Seq(j0.posForm, j0.negForm) contains idx.j))
        m(idx.bari)
      else
        if (!(Seq(j0.posForm, j0.negForm) contains idx.i) &
          (Seq(j0.posForm, j0.negForm) contains idx.j))
        m(idx.barj)
      else
        if ((Seq(j0.posForm, j0.negForm) contains idx.i) &
          (Seq(j0.posForm, j0.negForm) contains idx.j))
        m(idx.barj.bari)
      else {
        assert (
          !(Seq(j0.posForm, j0.negForm) contains idx.i) &
            !(Seq(j0.posForm, j0.negForm) contains idx.j))
        m(idx)})}

  // Cases 5, 6, 7 are implemented in the Octagon trait

  /////////////////////////////
  // End abstract assignments
  /////////////////////////////

  def union(that : Octagon[N]) : Octagon[N] = {
    that match {
      case bot : BottomOctagon[N] => { assert (bot.dimension == this.dimension); this }
      case o   : SimpleOctagon[N] => { assert (o.dimension == this.dimension);
        stronglyClosed(this.m union o.m)
      }
      case _ => ??? // Not implemented
    }
  }

  def intersection(that : Octagon[N]) : Octagon[N] = {
    that match {
      case bot : BottomOctagon[N] => { assert (bot.dimension == this.dimension); that }
      case o   : SimpleOctagon[N] => { assert (o.dimension == this.dimension);
        stronglyClosed(this.m intersection o.m)
      }
      case _ => ??? // Not implemented
    }
  }

  def widening(that : Octagon[N]) : Octagon[N] =
    that match {
      case bot : BottomOctagon[N] => this
      case o   : SimpleOctagon[N] => stronglyClosed(
        this.m.combine((us, them) =>
          if (us >= them) us
          else ∞
        )(o.m)
      )
    }

  def narrowing(that : Octagon[N]) : Octagon[N] =
    that match {
      case bot : BottomOctagon[N] => this
      case o   : SimpleOctagon[N] => stronglyClosed(
        this.m.combine((us, them) =>
          if (us == ∞)
            them
          else
            us
        )(o.m)
      )
    }

  /////////////////////////////////////////////

  /**
    * The indices *look* swapped here but are not.
    *
    * See Mine 2006 p.7, "the element at line i, column j, where 1 ≤ i
    * ≤ n, 1 ≤ j ≤ n, denoted by mij equals c ∈ I if there is a
    * constraint of the form Vj − Vi ≤ c ..."
    */
  def get_ineq_vi_minus_vj_leq_c(j : SignedVarIdx, i : SignedVarIdx) : Option[N] = Some(m(DBMIdx(i.i, j.i)))
  def isEmpty = isBottom
  def isBottom = false
  def isTop = m.isTop
  def dimension : OctagonDim = m.dimension

  def tryCompareTo[B >: Octagon[N]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
    other match {
      case them : Octagon[N] =>
        them match {
          case _ : BottomOctagon[N] => Some(1)
          case o : SimpleOctagon[N] =>
            this.m.tryCompareTo(o.m)
            // The following is only useful for making this comparable to "optimized" octagons
            // for debugging purposes
          case o : optimized.OptimizedOctagon[N] =>
            o.closedDbm.fold(Some(1) : Option[Int])( // It's bottom
              (m : ClosedDBM[N]) => this.m.tryCompareTo(m))
          case _ => ??? // Not impl
        }
      case _ => None
    }
}
