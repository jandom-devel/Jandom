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

package it.unich.jandom.domains.numerical.octagon.optimized
import it.unich.jandom.domains.numerical.octagon._
import it.unich.jandom.utils.dbm._
import it.unich.jandom.utils.numberext.IField
import it.unich.jandom.utils.numberext.StaticIField

import scala.language.implicitConversions

//////////////////////////////////////////////////////////////////
// Some sugar to make the following stuff read more like Mine 2006
//////////////////////////////////////////////////////////////////

object OctSugar {
  implicit def OctagonDimToDBMDim(d: OctagonDim) : DBMDim = d.toDBMDim
  implicit def DBMtoOctagonDim(d : DBMDim) : OctagonDim = {
    require(d.dbmDimToInt > 0 && (d.dbmDimToInt) % 2 == 0)
    OctagonDim(d.dbmDimToInt / 2)
  }
  case class OctIdx(idx : DBMIdx) {
    def i = SignedVarIdx(idx.i)
    def j = SignedVarIdx(idx.j)
  }
  implicit def octftodbmf[N] (f : OctIdx => N) : DBMIdx => N = (idx : DBMIdx) => f(OctIdx(idx))
  implicit def octidxtodbmidx (idx : OctIdx) : DBMIdx = idx.idx
}

//////////////////////////////
// End sugar
//////////////////////////////


/**
  * An abstract implementation of an octagonal representation that
  * uses a DBM as its backend (basically, this _is_ a Mine'-style
  * octagon, the artificial separation is for convenience of
  * e.g. testing, stubbing).
  *
  * Leverages the Template Method pattern to inject different closure
  * behaviours.
  *
  * It allows to optimize the implementation by:
  *
  * 1. Keeping a non-closed inner matrix and performing the closure on
  *    request
  *
  * 2. Performing an incremental or noop closure when there is proof
  *    of its sufficiency (i.e. those operations which are known to
  *    always yield a closed DBM)
  */

trait OptimizedOctagon[N <: IField[N]] extends Octagon[N] {
  import OctSugar.octftodbmf
  import OctSugar.octidxtodbmidx
  import OctSugar.OctIdx

  protected def ifield : StaticIField[N]

  //////////////////////////////////////////////////////////////
  // Utilities relative to implementation and delaying behaviour
  //////////////////////////////////////////////////////////////

  /**
    * A closed version of the underlying DBM for this Octagon.  If the
    * implementation doesn't keep a closed DBM at all times, the
    * implementation of this method is to perform the closure.
    */
  protected[octagon] def closedDbm : Option[ClosedDBM[N]]
  protected[octagon] def anyDbm : DBM[N]

  protected[octagon] def onClosedDBM (f : ClosedDBM[N] => Octagon[N]) : Octagon[N]
  protected[octagon] def onAnyDBM (f : DBM[N] => Octagon[N]) : Octagon[N]
  protected[octagon] def onEither (ifNonClosed : DBM[N] => Octagon[N], ifClosed : ClosedDBM[N] => Octagon[N]) : Octagon[N]

  protected[octagon] def closeIncrementally (j0 : Var)(f : ClosedDBM[N] => DBMIdx => N) : ClosedDBM[N] => Octagon[N]
  protected[octagon] def yieldClosed (f : DBM[N] => DBMIdx => N) : DBM[N] => Octagon[N]
  protected[octagon] def yieldClosed (dbm : DBM[N]) : Octagon[N]
  protected[octagon] def yieldNonClosed (f : DBM[N] => DBMIdx => N) : DBM[N] => Octagon[N]
  protected[octagon] def yieldNonClosed (dbm : DBM[N]) : Octagon[N]

  protected[octagon] def preserveClosure (f : DBM[N] => DBMIdx => N) =
    onEither(
      ifClosed = yieldClosed(f),
      ifNonClosed = yieldNonClosed(f))


  /////////////////////////////////////////////////////////////////////


  ////////////////////////////////////
  // Begin octagon transfer functions
  ////////////////////////////////////


  // If the argument is closed the result is guaranteed to ble closed already (Mine' 2006 Fig. 26 p. 54)
  def forget(f : Var) = preserveClosure(
    forget_f(_)(f)
  )

  def forget_f(m : DBM[N]) (f : Var) (idx : OctIdx) =
      if (
        idx.i != f.posForm &
          idx.i != f.negForm &
          idx.j != f.posForm &
          idx.j != f.negForm)
        m(idx) : N
      else if (idx.i == idx.j & idx.j == f.posForm | idx.i == idx.j & idx.j == f.negForm)
        ifield.zero : N
      else
        ifield.PositiveInfinity : N


  //////////////////
  // Abstract tests
  //////////////////

  def test_vj0_plus_c_le_0 (j0 : Var, c : N) : Octagon[N] = {
    // Case 1. Mine' 2006 fig. 20 p. 42
    // {{ Vj0 + c <= 0 ? }}

    def f (m : DBM[N]) (idx : OctIdx) =
      if (idx.i == j0.negForm & idx.j == j0.posForm)
        m(idx).min(-c._x_2)
      else
        m(idx)

    onEither(
      ifNonClosed = yieldNonClosed(f(_)),
      ifClosed = closeIncrementally(j0)(f(_)))

    // This test transfer function does not require a strongly closed
    // argument (Mine' 2006 p. 42)

    // If the argument is strongly closed, the result can be made
    // strongly closed in quadratic time by applying the incremental
    // strong closure The same applies to all exact octagonal tests.

    // (Mine' 2006 p. 42)
  }

  def test_minus_vj0_plus_c_le_0 (j0 : Var, c : N) : Octagon[N] = {
    // Case 2. Mine' 2006 fig. 20 p. 42
    // {{ -Vj0 + c <= 0 ? }}

    def f (m : DBM[N])(idx : OctIdx) =
      if (idx.i == j0.posForm & idx.j == j0.negForm)
        m(idx).min(-c._x_2)
      else
        m(idx)

    onEither( // See comment in test_vj0_plus_c_le_0
      ifNonClosed = yieldNonClosed(f(_)),
      ifClosed = closeIncrementally(j0)(f(_)))
  }

  def test_vj0_minus_vi0_plus_c_le_0(j0 : Var, i0 : Var, c : N) : Octagon[N] = {
    // Case 3. Mine' 2006 fig. 20 p. 42
    // {{ -Vj0 + Vi0- + c <= 0 ? }}

    require(j0 != i0) // see fig. 20

    def f (m :DBM[N]) (idx : OctIdx) =
    if (idx.i == i0.posForm & idx.j == j0.posForm
      | idx.i == j0.negForm & idx.j == i0.negForm)
      m(idx).min(-c)
    else
      m(idx)

    onEither( // See comment in test_vj0_plus_c_le_0
      ifNonClosed = yieldNonClosed(f(_)),
      ifClosed = closeIncrementally(j0)(f(_)))
  }


  def test_vj0_plus_vi0_le_c(j0 : Var, i0 : Var, c : N) : Octagon[N] = {
    // Case 4. Mine' 2006 fig. 20 p. 42
    // {{ Vj0 + Vi0- + c <= 0 ? }}
    require(j0 != i0) // see fig. 20

    def f (m : DBM[N]) (idx : OctIdx) =
      if (idx.i == i0.negForm & idx.j == j0.posForm
        | idx.i == j0.negForm & idx.j == i0.posForm)
        m(idx).min(-c)
      else
        m(idx)

    onEither( // See comment in test_vj0_plus_c_le_0
      ifNonClosed = yieldNonClosed(f(_)),
      ifClosed = closeIncrementally(j0)(f(_)))
  }


  def test_minus_vj0_minus_vi0_plus_c_le_0(j0 : Var, i0 : Var, c : N) : Octagon[N] = {
    // Case 5. Mine' 2006 fig. 20 p. 42
    // {{ -Vj0 + Vi0- + c <= 0 ? }}
    require(j0 != i0)

    def f(m : DBM[N]) (idx : OctIdx) =
        if (idx.i == i0.posForm & idx.j == j0.negForm
          | idx.i == j0.posForm & idx.j == i0.negForm)
          m(idx).min(-c)
        else
          m(idx)

    onEither( // See comment in test_vj0_plus_c_le_0
      ifNonClosed = yieldNonClosed(f(_)),
      ifClosed = closeIncrementally(j0)(f(_)))
  }

  ///////////////////////
  // Abstract assignment
  ///////////////////////

  def assign_vj0_gets_c(j0 : Var, c : N) : Octagon[N] =
    // Case 1. Mine' 2006 fig. 15 p. 35
    // {{ Vj0 <- c }}
    onClosedDBM(
      closeIncrementally(j0)(
      // Non-invertible assignments require a strongly closed argument
      // [...]  due to the embedded forget operator, [...] the result
      // can be strongly closed by performing incremental closure wrt
      // the assigned variable (Mine' 2006 p.36)
      (m : ClosedDBM[N]) => (idx : OctIdx) =>
          if (idx.i == j0.posForm & idx.j == j0.negForm)
            (-c._x_2)
          else if (idx.i == j0.negForm & idx.j == j0.posForm)
            (c._x_2)
          else
            forget_f(m)(j0)(idx)))

  def assign_vj0_gets_vj0_plus_c(j0 : Var, c : N) : Octagon[N] =
    // Case 2. Mine' 2006 fig. 15 p. 35
    // {{ Vj0 <- Vj0 + c }}
    preserveClosure(
      // Invertible assignments do not require strongly closed
      // matrix arguments but preserve the strong closure (Mine'
      // 2006 p. 34)
      (m : DBM[N]) => (idx : OctIdx) =>
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



  def assign_vj0_gets_minus_vj0(j0 : Var) : Octagon[N] =
    // Case 4. Mine' 2006 fig. 15 p. 35
    // {{ Vj0 <- -Vj0 }}
    preserveClosure(
      // Invertible assignments do not require strongly closed
      // matrix arguments but preserve the strong closure (Mine'
      // 2006 p. 34)
      (m : DBM[N]) => (idx : OctIdx) =>
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
          require (
            !(Seq(j0.posForm, j0.negForm) contains idx.i) &
            !(Seq(j0.posForm, j0.negForm) contains idx.j))
          m(idx)})

  def assign_vj0_gets_vi0_plus_c(j0 : Var, i0 : Var, c : N) : Octagon[N] = {
    // Case 3. Mine' 2006 fig. 15 p. 35
    // {[ Vj0 <- Vi0 + c }}
    require (j0 != i0)
    onClosedDBM(
      closeIncrementally(j0)(
      // Non invertible assignments [like] Vj0 ← ±Vi0 + [a, b] [...]
      // require a strongly closed argument due to the embedded
      // forget operator; [...] can be closed by merely performing
      // an incremental strong closure with respect to the assigned
      // variable Vj0 (Mine' 2006 p.36).
      (m : ClosedDBM[N]) => (idx : OctIdx) =>
        if (idx.i == j0.posForm & idx.j == i0.posForm
          | idx.i == i0.negForm  & idx.j == j0.negForm)
          -c
        else
          if (idx.i == i0.posForm & idx.j == j0.posForm
            | idx.i == j0.negForm  & idx.j == i0.negForm)
          c
        else
          forget_f(m)(j0)(idx)))}

  /////////////////////////////////////

  /**
    *  The indices *look* swapped here but are not.  Cfr Mine 2006
    *  p.7, "the element at line i, column j, where 1 ≤ i ≤ n, 1 ≤ j ≤
    *  n, denoted by mij equals c ∈ I if there is a constraint of the
    *  form Vj − Vi ≤ c ..."
    */
  def get_ineq_vi_minus_vj_leq_c(j : SignedVarIdx, i : SignedVarIdx) : Option[N] = closedDbm.map(_(DBMIdx(i.i, j.i)))

  def isEmpty = isBottom

  def isBottom = closedDbm match {
    case Some(_) => false
    case None => true
  }

  def isTop = closedDbm.map(_.isTop) match {
    case Some(t) => t
    case None => false
  }

  def dimension : OctagonDim

  def union(that : Octagon[N]) : Octagon[N] = {
    that match {
      case bot : BottomOctagon[N] => { assert (bot.dimension == this.dimension); this }
      case o : OptimizedOctagon[N] => { assert (o.dimension == this.dimension);
        o.closedDbm match {
          case Some(them) =>
            this.closedDbm match {
              case Some(us) => yieldClosed(us union them)
              // Result of union of two closed DBMs is closed (Mine' 2006 Fig. 27 p. 54)
              case None => BottomOctagon[N](this.dimension)
            }
          case _ => BottomOctagon[N](this.dimension)
        }
      }
      case _ => ??? // Not implemented
    }
  }

  def intersection(that : Octagon[N]) : Octagon[N] = {
    that match {
      case bot : BottomOctagon[N] => { assert (bot.dimension == this.dimension); that }
      case o : OptimizedOctagon[N] => { assert (o.dimension == this.dimension);
        yieldNonClosed(o.anyDbm.intersection(this.anyDbm))
        // Closure is not guaranteed even if operands are closed,
      }
      case _ => ??? // Not implemented
    }
  }

  def widening(that : Octagon[N]) : Octagon[N] =
    if (that.isBottom)
      this
    else
      that match {
        case bot : BottomOctagon[N] => this
        case o   : OptimizedOctagon[N] =>
          this.closedDbm.flatMap(us =>
            o.closedDbm.map(
              them =>
              us.combine((us, them) =>
                if (us >= them) us
                else ifield.PositiveInfinity)(them))) match {
            case None => BottomOctagon(dimension)
            case Some(dbm) => yieldNonClosed(dbm)
          }
      }

  def narrowing(that : Octagon[N]) : Octagon[N] =
    if (that.isBottom)
      that
    else
      that match {
        case bot : BottomOctagon[N] => that
        case o   : OptimizedOctagon[N] =>
          this.closedDbm.flatMap(us =>
            o.closedDbm.map(
              them =>
              us.combine((us, them) =>
                if (us == ifield.PositiveInfinity) them
                else us)(them))) match {
            case None => BottomOctagon(dimension)
            case Some(dbm) => yieldNonClosed(dbm)
          }
     }




  ////////////////////////////////////////

  def tryCompareTo[B >: Octagon[N]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = {
    this.closedDbm match {
      case Some(us) => other match {
        case _ : BottomOctagon[N] => Some(1) // They're bottom, we're greater
        case o : OptimizedOctagon[N] => o.closedDbm match {
          case Some(them) => us.tryCompareTo(them)
          case None => Some(1) // They're bottom, we're greater
        }
        //// Additional case for comparing with SimpleOctagons
        // case s : simple.SimpleOctagon[N] => us.tryCompareTo(s.m)
        ////
        case _ => ??? // Not impl
      }
      case None => other match {
        case _ : BottomOctagon[N] => Some(0) // They're bottom,
                                             // we're also bottom
        case o : OptimizedOctagon[N] => o.closedDbm match {
          case Some(them) => Some(-1) // They're greater, we're
                                      // bottom
          case None => Some(0)        // Both bottom
        }
        //// Additional case for comparing with SimpleOctagons
        // case s : simple.SimpleOctagon[N] => Some(-1) // They're a
                                                        // closed
                                                        // non-bottom
                                                     // DBM, we
                                                     // are
                                                     // bottom
        case _ => ??? // Not impl
      }
    }
  }
}

///////////////////
// Implementations
///////////////////


/**
  * This avoids optimizations and, particularly, mantains the
  * invariant that dbm is closed by doing a full closure everytime,
  * even after closure-preserving operations.
  *
  * In other words, it ought to behave like SimpleOctagon from the
  * .simple package
  *
  * Useful (?) for debugging.
  *
  * You can extend OptimizedOctagon for different behaviours - e.g. keep the
  * laziness, but do full closure instead of incremental closure and
  * so on.
  */
class PedanticOctagon[N <: IField[N]]
  (val dbm : ClosedDBM[N])
  (implicit val ifield : StaticIField[N], cs : MineFloydWarshall[N], fac : DBMFactory[N]) extends OptimizedOctagon[N] {

  protected def assign_vj0_gets_vi0(j0 : Var, i0 : Var) : Octagon[N] = assign_vj0_gets_vi0_plus_c(j0, i0, ifield.zero)

  /////////////

  protected[octagon] def anyDbm : DBM[N] = dbm
  protected[octagon] def closedDbm : Option[ClosedDBM[N]] = Some(dbm)

  protected def wrap(dbm : Option[ClosedDBM[N]]) : Octagon[N] = dbm match {
    case Some (closed) => new PedanticOctagon[N](closed)
    case None => BottomOctagon(dimension)
  }

  protected[octagon] def onClosedDBM (f : ClosedDBM[N] => Octagon[N]) : Octagon[N] = f(dbm)

  protected[octagon] def onAnyDBM (f : DBM[N] => Octagon[N]) : Octagon[N] = f(dbm)

  protected[octagon] def onEither (ifNonClosed : DBM[N] => Octagon[N], ifClosed : ClosedDBM[N] => Octagon[N]) : Octagon[N] = ifClosed(dbm)


  protected[octagon] def closeIncrementally (j0 : Var)(f : ClosedDBM[N] => DBMIdx => N) : ClosedDBM[N] => Octagon[N] = {
    (closed : ClosedDBM[N]) => wrap(cs.strongClosure(fac.fromFun(dbm.dimension, f(closed))))
  }
  protected[octagon] def yieldClosed (f : DBM[N] => DBMIdx => N) : DBM[N] => Octagon[N] = {
    (dbm : DBM[N]) => wrap(cs.strongClosure(fac.fromFun(dbm.dimension, f(dbm))))
  }
  protected[octagon] def yieldClosed (dbm : DBM[N]) : Octagon[N] = {
    wrap(cs.strongClosure(dbm))
  }
  protected[octagon] def yieldNonClosed (f : DBM[N] => DBMIdx => N) : DBM[N] => Octagon[N] = {
    (dbm : DBM[N]) => wrap(cs.stronglyClosed(dbm.dimension)(f(dbm)))
  }
  protected[octagon] def yieldNonClosed (dbm : DBM[N]) : Octagon[N] = {
    wrap(cs.strongClosure(dbm))
  }

  def dimension = OctagonDim(dbm.dimension)
}

/**
  * An implementation of OptimizedOctagon that tries to delay closure
  * until it's a prerequisite
  */
class DelayedOctagon[N <: IField[N]]
  (val dbm : Either[DBM[N], ClosedDBM[N]])
  (implicit val ifield : StaticIField[N], cs : IncrementalMineFloydWarshall[N], fac : DBMFactory[N]) extends OptimizedOctagon[N] {

  protected def assign_vj0_gets_vi0(j0 : Var, i0 : Var) : Octagon[N] =
    assign_vj0_gets_vi0_plus_c(j0, i0, ifield.zero)

  protected[octagon] def anyDbm : DBM[N] = dbm match {
    case Left(any) => any
    case Right(closed) => closed
  }

  // Possible optimization: let dbm be mutable and cache the closed dbm
  protected[octagon] def closedDbm : Option[ClosedDBM[N]] = this.dbm match {
    case Left(any) => cs.strongClosure(any)
    case Right(closed) => Some(closed)
  }


  protected def wrapAny(dbm : DBM[N]) : Octagon[N] = new DelayedOctagon[N](Left(dbm))
  protected def wrapClosed(maybe : Option[ClosedDBM[N]]) : Octagon[N] = maybe match {
    case None => BottomOctagon(dimension)
    case Some(closed) => new DelayedOctagon[N](Right(closed))
  }

  protected[octagon] def onClosedDBM (f : ClosedDBM[N] => Octagon[N]) : Octagon[N] = closedDbm match {
    case Some(closed) => f(closed)
    case None => BottomOctagon(dimension)
  }

  protected[octagon] def onAnyDBM (f : DBM[N] => Octagon[N]) : Octagon[N] = {
    f(anyDbm)
  }

  protected[octagon] def onEither (ifNonClosed : DBM[N] => Octagon[N], ifClosed : ClosedDBM[N] => Octagon[N]) : Octagon[N] =
    dbm match {
      case Left(anyDbm) => ifNonClosed(anyDbm)
      case Right(closed) => ifClosed(closed)
    }

  private def dbmDimension = anyDbm.dimension

  protected[octagon] def closeIncrementally (j0 : Var)(f : ClosedDBM[N] => DBMIdx => N) : ClosedDBM[N] => Octagon[N] = {
    (closed : ClosedDBM[N]) => wrapClosed(cs.incrementallyClosed(j0)(dbmDimension)(f(closed)))
  }
  protected[octagon] def yieldClosed (f : DBM[N] => DBMIdx => N) : DBM[N] => Octagon[N] = {
    (dbm : DBM[N]) => wrapClosed(Some(cs.makeClosed(dbmDimension)(f(dbm))))
  }
  protected[octagon] def yieldClosed (dbm : DBM[N]) : Octagon[N] = {
    wrapClosed(Some(cs.markAsClosed(dbm)))
  }

  protected[octagon] def yieldNonClosed (f : DBM[N] => DBMIdx => N) : DBM[N] => Octagon[N] = {
    (dbm : DBM[N]) => wrapAny(fac.fromFun(dbmDimension, f(dbm)))
  }
  protected[octagon] def yieldNonClosed (dbm : DBM[N]) : Octagon[N] = {
    wrapAny(dbm)
  }

  def dimension = OctagonDim(anyDbm.dimension)
}



/**
  * This one is like DelayedOctagon but is mutable and caches closure.
  */
class CachingOctagon[N <: IField[N]]
 (dbm : Either[DBM[N], ClosedDBM[N]])
  (implicit ifield : StaticIField[N],
    cs : IncrementalMineFloydWarshall[N],
    fac : DBMFactory[N]) extends DelayedOctagon[N](dbm)(ifield, cs, fac){

  var cached_closed : Option[Option[ClosedDBM[N]]] = None

  override protected def wrapAny(dbm : DBM[N]) : Octagon[N] = new CachingOctagon[N](Left(dbm))
  override protected def wrapClosed(maybe : Option[ClosedDBM[N]]) : Octagon[N] = maybe match {
    case None => BottomOctagon(dimension)
    case Some(closed) => new CachingOctagon[N](Right(closed))
  }

  protected[octagon] override def anyDbm : DBM[N] = closedDbm match {
    case Some(closed) => closed
    case None => super.anyDbm
  }

  protected[octagon] override def closedDbm : Option[ClosedDBM[N]] = cached_closed match {
    case None => this.dbm match {
      case Left(any) => {
        val res = cs.strongClosure(any)
        cached_closed = Some(res)
        res
      }
      case Right(closed) => Some(closed)
    }
    case Some(maybeclosed) => maybeclosed
  }
}
