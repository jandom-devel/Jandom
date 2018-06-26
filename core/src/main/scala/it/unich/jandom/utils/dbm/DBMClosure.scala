/**
  * Copyright 2017 Filippo Sestini, Tobia Tesan
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

package it.unich.jandom.utils.dbm
import it.unich.jandom.utils.numberext.IField
import it.unich.jandom.utils.numberext.StaticIField

/**
  * An implementation of the modified Floyd-Warshall algorithm found
  * in Mine' 2006 figure 9 p. 20, which computes the strong closure of
  * a DBM.
  */
class MineFloydWarshall[N <: IField[N]](implicit ifield: StaticIField[N], val d : DBMFactory[N]) {

  import scala.language.implicitConversions

  ////////////////
  // Begin sugar
  ////////////////

  import it.unich.jandom.domains.numerical.octagon.SignedVarIdx
  import it.unich.jandom.domains.numerical.octagon.VPlusIdx
  import it.unich.jandom.domains.numerical.octagon.VMinusIdx
  import it.unich.jandom.domains.numerical.octagon.Var

  implicit def signedvartoint(i : SignedVarIdx) : Int =
    i match {
      case VPlusIdx(i) => i
      case VMinusIdx(i) => i
      case _ => ???
    }

  implicit def inttovar(i : Int) : Var = Var(i)

  ////////////////
  // End sugar
  ////////////////


  def stronglyClosed (n : DBMDim)(f : DBMIdx => N): Option[ClosedDBM[N]] =
    strongClosure(
      d.fromFun(n,f)
    )


  def strongClosure (m: DBM[N]): Option[ClosedDBM[N]] = {
    val n2 = m.dimension.dbmDimToInt
    val n = n2 / 2

    def step1(k : Int, n2 : Int, m : DBM[N]) = {
      (1 to n2).flatMap(
        i =>
        (1 to n2).map(
          j => (Var(k), SignedVarIdx(i), SignedVarIdx(j))
        )
      ).foldRight(m)(closeIter)
    }

    def strengtheningStep(n2 : Int, m : DBM[N]) = {
      (1 to n2).flatMap(
        i =>
        (1 to n2).map(
          j => (SignedVarIdx(i), SignedVarIdx(j))
        )
      ).foldRight(m)(strengthenIter)
    }

    emptinessCheck(
      (1 to n).foldRight(m)(
        (k, m) => strengtheningStep(n2, step1(k, n2, m))
      )).map(markAsClosed(_))
  }



  protected def closeIter (kij : (Var, SignedVarIdx, SignedVarIdx), m : DBM[N]) = {
    val k = kij._1
    val i = kij._2
    val j = kij._3
    m.updated(DBMIdx(i, j))(
      m(DBMIdx(i, j)).min(
        m(DBMIdx(i, k.posForm)) + m(DBMIdx(k.posForm, j))).min(
        m(DBMIdx(i, k.negForm)) + m(DBMIdx(k.negForm, j))).min(
        m(DBMIdx(i, k.posForm)) + m(DBMIdx(k.posForm, k.negForm)) + m(DBMIdx(k.negForm, j))).min(
        m(DBMIdx(i, k.negForm)) + m(DBMIdx(k.negForm, k.posForm)) + m(DBMIdx(k.posForm, j)))
    )
  }

  protected def strengthenIter (ij : (SignedVarIdx, SignedVarIdx), m : DBM[N]) : DBM[N] = {
      val i = ij._1
    val j = ij._2

      m.updated(DBMIdx(i, j))(
        m(DBMIdx(i, j)).min(
          (m(DBMIdx(i, i).barj) + m(DBMIdx(j, j).bari))._div_2))
  }

  protected def emptinessCheck(m: DBM[N]): Option[DBM[N]] = {
    if (m.all.exists((pair : (DBMIdx, N)) => pair._1.diagonal & pair._2 < ifield.zero))
      None
    else {
      val updater: DBMIdx => N =
        (idx =>
          if (idx.diagonal)
            ifield.zero
          else
            m(idx))
      Some(d.fromFun(m.dimension, updater))
    }
  }
  def makeClosed(n : DBMDim)(f : DBMIdx => N) : ClosedDBM[N] = d.markAsClosed(d.fromFun(n, f))
  def markAsClosed(dbm : DBM[N]) : ClosedDBM[N] = d.markAsClosed(dbm)
}


// TODO: Deduplicate

// TODO: Add comments!

class IncrementalMineFloydWarshall[N <: IField[N]] (implicit ifield: StaticIField[N], d : DBMFactory[N]) extends MineFloydWarshall[N]()(ifield, d) {
  import it.unich.jandom.domains.numerical.octagon.Var
  import it.unich.jandom.domains.numerical.octagon.SignedVarIdx

  def incrementallyClosed (j0 : Var)(n : DBMDim)(f : DBMIdx => N): Option[ClosedDBM[N]] =
    incrementalClosure(j0)(
      d.fromFun(n,f)
    )

  def incrementalClosure (j0 : Var)(m: DBM[N]): Option[ClosedDBM[N]] = {
    val n2 = m.dimension.dbmDimToInt
    val n = n2 / 2

    def step1(k : Int, n2 : Int, m : DBM[N]) = {
      (1 to n2).flatMap(
        i =>
        (1 to n2).map(
          j => (Var(k), SignedVarIdx(i), SignedVarIdx(j))
        )
      ).filter(ijk =>
        ijk._1 == j0 |
        ijk._2 == j0.posForm | ijk._2 == j0.negForm |
          ijk._3 == j0.posForm | ijk._3 == j0.negForm).foldRight(m)(closeIter)
    }

    def strengtheningStep(k : Int, n2 : Int, m : DBM[N]) = {
      (1 to n2).flatMap(
        i =>
        (1 to n2).map(
          j => (SignedVarIdx(i), SignedVarIdx(j))
        )
      ).filter(ijk =>
        k == j0.i |
        ijk._1 == j0.posForm | ijk._1 == j0.negForm |
          ijk._2 == j0.posForm | ijk._1 == j0.negForm).foldRight(m)(strengthenIter)
    }

    val allbutk = (1 to n).filter(_ != j0.i).foldRight(m)(
      (k, m) => strengtheningStep(k, n2, step1(k, n2, m)))


    /* As a LAST step we update the WHOLE matrix for all (i, j0), (j0, j).
     * See Mine''s PhD thesis p.75:
     * "In the first c iterations, the algorithm only updates the last n −
     *  c lines and columns of n.  The last n − c iterations update
     *  the whole matrix, as the regular Floyd–Warshall algorithm
     *  does."
     */
    val kthstep = strengtheningStep(j0.i, n2, step1(j0.i, n2, allbutk))

      emptinessCheck(
        kthstep
      ).map(markAsClosed(_))

  }
}
