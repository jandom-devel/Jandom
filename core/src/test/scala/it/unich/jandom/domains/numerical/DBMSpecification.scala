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
  * but WITHOUT ANY WARRANTY; without even the implied warranty of a
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
  */

package it.unich.jandom.utils.dbm
import scala.reflect.ClassTag
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import it.unich.jandom.utils.numberext.RationalExt

class DBMSpecification extends AnyPropSpec with ScalaCheckPropertyChecks {
  // TODO: write real tests!
  implicit val ifield = RationalExt
  implicit val tag = implicitly[ClassTag[RationalExt]]
  implicit val afac = new ArrayDBMFactory()(tag, ifield)
  implicit val closure = new IncrementalMineFloydWarshall[RationalExt]()(ifield, afac)

  property("Quick check that closure does not break a closed DBM") {
    // TODO: Test it for real
    val arr = Array(
      Array(RationalExt.zero, RationalExt.PositiveInfinity),
      Array(RationalExt.PositiveInfinity, RationalExt.zero))
    val dbm = ArrayDBM(DBMDim(2), arr)
    assert(closure.strongClosure(dbm).get == afac.markAsClosed(dbm))
    val arr2 = Array(
      Array(RationalExt.zero, RationalExt.zero),
      Array(RationalExt.zero, RationalExt.zero))
    val dbm2 = ArrayDBM(DBMDim(2), arr2)
    assert(closure.strongClosure(dbm2).get == afac.markAsClosed(dbm2))
    val arr3 = Array(
      Array(RationalExt.zero, RationalExt(1)),
      Array(RationalExt(1), RationalExt.zero))
    val dbm3 = ArrayDBM(DBMDim(2), arr3)
    assert(closure.strongClosure(dbm3).get == afac.markAsClosed(dbm3))
    val arr4 = Array(
      Array(RationalExt(1), RationalExt(1)),
      Array(RationalExt(1), RationalExt(1)))
    val dbm4 = ArrayDBM(DBMDim(2), arr4)
    assert(closure.strongClosure(dbm4).get != afac.markAsClosed(dbm4))
    val arr5 = Array(
      Array(RationalExt(0), RationalExt(-1)),
      Array(RationalExt(1), RationalExt(0)))
    val dbm5 = ArrayDBM(DBMDim(2), arr5)
    assert(closure.strongClosure(dbm5).get == afac.markAsClosed(dbm5))
  }


  property ("Quick check that closure can tell a bottom when it sees it ") {
    val a = afac.fromFun(DBMDim(4), (idx : DBMIdx) =>
      if (idx.i == 2 & idx.j == 1 |idx.i == 1 & idx.j == 2)
        -2
      else if (idx.diagonal)
        0
      else
        ifield.PositiveInfinity)

    assert(closure.strongClosure(a) == None)
  }


  property ("Quick check for incrementalClosure") {

    // TODO: write real tests that incrementalClosure(j0) yields the
    // same as regular strong closure after changing the j0-th
    // row/column on a closed DBM

    val a = afac.fromFun(DBMDim(4), (idx : DBMIdx) =>
      if (idx.i == 2 & idx.j == 1)
        2
      else if (idx.i == 1 & idx.j == 2)
        1
      else if (idx.i == 3 & idx.j == 4)
        1
      else if (idx.i == 4 & idx.j == 3)
        -1
      else if (idx.i == 5 & idx.j == 6)
        0
      else if (idx.i == 6 & idx.j == 5)
        0
      else if (idx.diagonal)
        0
      else
        ifield.PositiveInfinity)

    /*
      0 -1
      1  0
           0  1
          -1  0
                 0  0
                 0  0
     */

    val closed = closure.strongClosure(a)
    assert(closed != None)
    import it.unich.jandom.domains.numerical.octagon.Var
    val v2 = Var(2)
    val b = afac.fromFun(DBMDim(4), (idx : DBMIdx) =>
      if (idx.i == v2.posForm.i | idx.j == v2.posForm.i)
        a(idx) * 2
      else
        a(idx))


    val reclosed = closure.strongClosure(b)
    val incclosed = closure.incrementalClosure(v2)(b)
    assert(reclosed == incclosed)
  }
}
