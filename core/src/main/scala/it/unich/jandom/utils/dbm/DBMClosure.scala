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
