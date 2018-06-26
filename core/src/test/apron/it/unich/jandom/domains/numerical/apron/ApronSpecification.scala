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

package it.unich.jandom.domains.numerical.apron
import it.unich.jandom.domains.numerical._
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import spire.math.Rational
import it.unich.jandom.domains.numerical._
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.utils.numberext.RationalExt
import scala.language.reflectiveCalls

class ApronSpecification extends PropSpec with PropertyChecks {
  import it.unich.jandom.domains.numerical.Utils.Ints._
  import it.unich.jandom.domains.numerical.Utils.Rationals._
  import it.unich.jandom.domains.numerical.Utils.RationalExts._
  import it.unich.jandom.domains.numerical.Utils.LinearForms._

  val apronDomain = ApronIntOctagonDomain()
  ///////////////////////////////////////////////////////
  // Properties of representation-converting helpers
  ///////////////////////////////////////////////////////

  property("coeffToRational . rationalToCoeff == id") {
    forAll {
      (r: Rational)
      =>
      assert(SpireGmpUtils.unsafeCoeffToRational(SpireGmpUtils.rationalToCoeff(r)) == r)
    }
  }

  property("scalarToRatExt . ratExtToScalar == id") {
    forAll {
      (r: RationalExt)
      =>
      assert(SpireGmpUtils.scalarToRatExt(SpireGmpUtils.ratExtToScalar(r)) == r)
    }
  }

  property("linexprToLf . lfToLinexpr == id") {
    forAll {
      (r: LinearForm)
      =>
      assert(SpireGmpUtils.linexprToLf(SpireGmpUtils.lfToLinexpr(r)) == r)
    }
  }

  ///////////////////////////////////////////////////////
  // Basic properties of Apron octagons
  ///////////////////////////////////////////////////////


  property("top.isTop") {
    forAll (GenTinyPosInt){
      n : Int
      =>
      val top = apronDomain.top(n)
      assert (top.isTop)
    }
  }

  property("bottom.isBottom") {
    forAll (GenTinyPosInt){
      n : Int
      =>
      val bottom = apronDomain.bottom(n)
      assert (bottom.isBottom)
    }
  }

  property("T U T = T & T U _|_ = T") {
    forAll (GenTinyPosInt){
      n : Int
      =>
      val top = apronDomain.top(n)
      val bottom = apronDomain.bottom(n)
      assert (top.union(top) == top)
      assert (bottom.union(top) == top)
    }
  }

  property("T /\\ _|_ = _|_ & _|_ /\\ _|_ = _|_") {
    forAll (GenTinyPosInt){
      n : Int
      =>
      val top = apronDomain.top(n)
      val bottom = apronDomain.bottom(n)
      assert (bottom.intersection(bottom) == bottom)
      assert (top.intersection(bottom) == bottom)
    }
  }

  property("T U _|_ >= _|_ & T /\\ _|_ =< T") {
    forAll (GenTinyPosInt){
      n : Int
      =>
      val top = apronDomain.top(n)
      val bottom = apronDomain.bottom(n)
      assert (bottom.union(top) > bottom)
      assert (top.union(bottom) > bottom)
      assert (bottom.intersection(top) < top)
      assert (top.intersection(bottom) < top)
      assert (bottom.union(top) >= bottom)
      assert (top.union(bottom) >= bottom)
      assert (bottom.intersection(top) <= top)
      assert (top.intersection(bottom) <= top)
    }
  }

  property("T != _|_, T !< T, _|_ !< _|_") {
    forAll (GenTinyPosInt){
      n : Int
      =>
      val top = apronDomain.top(n)
      val bottom = apronDomain.bottom(n)
      assert (!(top == bottom))
      assert (!(bottom == top))
      assert (!(top > top))
      assert (!(top < top))
      assert (!(bottom < bottom))
      assert (!(bottom > bottom))
    }
  }

  property("_|_ <= T.{x <- c} <= T ") {
    import org.scalacheck.Shrink
    implicit val noShrink: Shrink[Int] = Shrink.shrinkAny
    forAll (Gen.choose(1, 50)){
      n
      =>
      forAll (Gen.posNum[Int]){
        ip
        =>
        forAll (Gen.choose(0, n)) {
          x : Int =>
          val i = ip%n
          val top = apronDomain.top(n)
          val bottom = apronDomain.bottom(n)
          val lf = LinearForm.c(spire.math.Rational(x,1))
          val updated = top.linearAssignment(i, lf)
          assert((updated.toInterval).low(i) == x)
          assert((updated.toInterval).high(i) == x)
          assert(updated <= top)
          assert(bottom <= updated)
        }
      }
    }
  }
}
