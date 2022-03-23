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

package it.unich.jandom.utils.dbm
import it.unich.jandom.utils.numberext.IField
import it.unich.jandom.utils.numberext.StaticIField
import scala.reflect.ClassTag

//////////////////////
// Utils
//////////////////////

case class DBMIdx(val i : Int, val j : Int) {
  require(i != 0)
  require(j != 0) // Mine' counts from 1, see e.g. fig 4.4 p 120
  private def bar(i: Int) = if (i % 2 == 0) i - 1 else i + 1 // Mine' 2006 counts indices from 1
  def diagonal : Boolean = (i == j)
  def bari = DBMIdx(bar(i), j)
  def barj = DBMIdx(i, bar(j))
}

case class DBMDim(private val n : Int) {
  require(n != 0)
  require(n % 2 == 0)
  def grid : Seq[Seq[DBMIdx]] = (1 to n).map(i => (1 to n).map(j => DBMIdx(i,j)))
  def allIdxs : Seq[DBMIdx] = grid.flatten
  def diagonalIdxs : Seq[DBMIdx] = (1 to n).map(i => DBMIdx(i,i))
  def dbmDimToInt : Int = n
}

//////////////////////
// DBM
//////////////////////

/**
  * A matrix-based DBM.
  *
  * "matrix-based" means that \bot is explicitly not represented (in
  * practice, Option[DBM] with None where \bot would be makes more
  * sense in terms of readability in the client classes)
  */
trait DBM[N <: IField[N]] {

  def isTop : Boolean

  def dimension : DBMDim

  def updated(idx : DBMIdx)(n : N) : DBM[N]

  protected def isClosed = false

  /**
    * Unlike union, the result of an intersection is seldom closed
    * (see Mine' 2006 p. 68)
    */
  def intersection (that : DBM[N]) : DBM[N] = {
    that match {
      case that : DBM[N] => this.combine(_.min(_))(that)
      case _ => ???
    }
  }

  def combine(f: (N, N) => N)(mb: DBM[N]): DBM[N]

  def apply(i : DBMIdx) : N

  def all : Seq[(DBMIdx, N)]

  def map[A](f : N => A) : Seq[A]
  override def toString : String =
    (if (this.isClosed) "Closed" else "") +
      "DBM(dim = " + dimension + ", mat = \n "+
  dimension.grid.map(row => row.map(idx =>
    this(idx).toString.take(5).padTo(5," ").mkString("")

  )
        .mkString("\t"))
      .mkString("\n")

  def canEqual(a: Any) = a.isInstanceOf[DBM[N @unchecked]]

  override def equals(that: Any): Boolean =
    that match {
      case that: DBM[N @unchecked] => {
        if (that.dimension == this.dimension)
          if (that.isClosed == this.isClosed)
            (dimension.allIdxs.forall(idx => that(idx) == this(idx)))
          else
            false
          else
            false
      }
      case _ => false
    }

  override def hashCode : Int = ???
}


trait ClosedDBM[N <: IField[N]] extends DBM[N] with PartiallyOrdered[ClosedDBM[N]] {
  /**
    * The union of closed DBM yields a closed DBM;
    * Figure 11 in Mine' 2006 p. 26;
    */
  def union(that : ClosedDBM[N]) : DBM[N] = {
    that match {
      // yields a closed DBM .get is therefore safe
      case that : ClosedDBM[N] => this.combine(_.max(_))(that)
      case _ => ???
    }
  }

  override def isClosed = true

  def tryCompareTo[B >: ClosedDBM[N]](other : B)(implicit arg0: (B) => PartiallyOrdered[B]) : Option[Int] =
    other match {
      case that : ClosedDBM[N @unchecked] => {
        require(that.dimension == this.dimension)
        if (dimension.allIdxs.forall(idx => that(idx) == this(idx))) Some(0)
        else if (dimension.allIdxs.forall(idx => this(idx) <= that(idx))) Some(-1) // We have tighter bounds
        else if (dimension.allIdxs.forall(idx => this(idx) >= that(idx))) Some(1) // They have tighter bounds
        else None
      }
      // We can't say anything until we close
      case _ => ??? // Option.empty
    }
}

trait DBMFactory[N <: IField[N]] {

  val ifield : StaticIField[N]

  def top(d : DBMDim) : ClosedDBM[N] = {
    markAsClosed(fromFun(d,
      idx => if (idx.diagonal) ifield.zero else ifield.PositiveInfinity
    ))
  }

  def fromFun(d : DBMDim, f : DBMIdx => N) : DBM[N]

  /**
    * This is for use ONLY by ourselves and Closure
    */
  protected[dbm] def markAsClosed(d : DBM[N]) : ClosedDBM[N]
}

///////////////////////////////////////////
// Quick & dirty array-based implementation
///////////////////////////////////////////


class ArrayDBMFactory[N <: IField[N]](implicit ctag : ClassTag[N], override val ifield : StaticIField[N]) extends DBMFactory[N]{

  def fromFun(d : DBMDim, f : DBMIdx => N) : ArrayDBM[N] = {
    (new ArrayDBM(d,
      d.allIdxs.foldLeft(
        Array.fill(d.dbmDimToInt)(Array.fill(d.dbmDimToInt)(ifield.PositiveInfinity))
      )(
        (z : Array[Array[N]], idx) => z.updated(idx.i - 1, z(idx.i - 1).updated(idx.j - 1, f(idx)))
          // Arrays are indexed from 0, Idxs start from 1 to match Mine' 2006
      )
    )(ctag, ifield))
  }

  protected[dbm] def markAsClosed(d : DBM[N]) : ClosedDBM[N] = {
    d match {
      case adbm : ArrayDBM[N] => (new ArrayDBM[N](adbm.dimension, adbm.a) with ClosedDBM[N])
      case dbm : DBM[N] => markAsClosed(fromFun(dbm.dimension, dbm(_)))
    }
  }
}

case class ArrayDBM[N <: IField[N]]
  (val dimension : DBMDim, val a : Array[Array[N]])
  (implicit ctag : ClassTag[N], val ifield : StaticIField[N]) extends DBM[N] {
  require(a.size == 0 | a.size > 0 & a(0).size == dimension.dbmDimToInt)

  def length = a.length

  override def isTop = dimension.allIdxs.forall(idx => idx.diagonal | a.apply(idx.i - 1).apply(idx.j - 1) == ifield.PositiveInfinity)

  /**
    * O(1)
    */
  def updated (idx : DBMIdx)(n : N) : DBM[N] = ArrayDBM(dimension, a.updated(idx.i - 1, a(idx.i - 1).updated(idx.j - 1, n)))

  /**
    * O(n^2)
    */
  override def combine(f: (N, N) => N)(that: DBM[N]): DBM[N] = {
    new ArrayDBM(dimension,
      dimension.grid.map(_.map(idx =>
        f(this(idx), that(idx))
      ).toArray).toArray)
  }

  /**
    * O(1)
    */
  def apply(idx : DBMIdx) : N = a.apply(idx.i - 1).apply(idx.j - 1)

  def all: Seq[(DBMIdx, N)] = dimension.allIdxs.map(idx => (idx, this(idx)))

  /**
    * O(n^2)
    */
  def map[A](f: N => A) : Seq[A] = dimension.allIdxs.map(idx => f(this(idx)))

  // TODO: This might be not such a good idea
  override def canEqual(a: Any) = super[DBM].canEqual(a)

  override def equals(that: Any): Boolean = super[DBM].equals(that)

  override def hashCode: Int = ??? // TODO
}
