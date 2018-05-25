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

package it.unich.jandom.domains.numerical
import it.unich.jandom.domains.numerical.octagon._
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import it.unich.jandom.domains.numerical.Utils.Ints._


class OctagonSpecification extends PropSpec with PropertyChecks {
  implicit val boxDomain : BoxRationalDomain = BoxRationalDomain()
  val octDomain = OctagonDomain()

  ///////////////////////////////////////////////////////
  // Sanity of to/fromBox
  ///////////////////////////////////////////////////////

  property("T_box to octagon to box == T_box") {
    forAll(GenTinyPosInt.suchThat(_ > 1)) {
      n => {
        val top = boxDomain.top(n)
        octDomain.fromBox(top).toBox == top
      }
    }
  }

  property("_|__box to octagon to box == _|__box") {
    forAll(GenTinyPosInt.suchThat(_ > 1)) {
      n => {
        val bottom = boxDomain.bottom(n)
        octDomain.fromBox(bottom).toBox == bottom
      }
    }
  }

  property("Some box to octagon to box == same box") {
    forAll(GenTinyPosInt.suchThat(_ > 1)) {
      n => {
        import Utils.Halfplanes._
        forAll(GenOctagonHalfplanesRat(n)) {
          h =>
          val box = octagonFromHalfPlanes(boxDomain)(n, h)
          assert(octDomain.fromBox(box).toBox == box)
        }
      }
    }
  }

  ///////////////////////////////////////////////////////
  // Sanity of helpers
  ///////////////////////////////////////////////////////

  property("Var(i).posForm.toVar == Var(i), simm. neg") {
    forAll(GenTinyPosInt.suchThat(_ > 0)) {
      i =>
      (Var(i).posForm).toVar == Var(i) &&
      (Var(i).negForm).toVar == Var(i)
    }
  }

  ///////////////////////////////////////////////////////
  // Basic ordering properties of Octagon octagons
  ///////////////////////////////////////////////////////


  property("top.isTop") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      val top = octDomain.top(n)
      assert (top.isTop)
      assert(top.toBox == boxDomain.top(n))
    }
  }

  property("bottom.isBottom") {
    forAll (GenTinyPosInt){
      n : Int
      =>
      val bottom = octDomain.bottom(n)
      assert (bottom.isBottom)
      assert(bottom.toBox == boxDomain.bottom(n))
    }
  }

  property("T U T = T & T U _|_ = T") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      val top = octDomain.top(n)
      val bottom = octDomain.bottom(n)
      assert (top.union(top) == top)
      assert (bottom.union(top) == top)
    }
  }

  property("T /\\ _|_ = _|_ & _|_ /\\ _|_ = _|_") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      val top = octDomain.top(n)
      val bottom = octDomain.bottom(n)
      assert (bottom.intersection(bottom) == bottom)
      assert (top.intersection(bottom) == bottom)
    }
  }

  property("T U _|_ >= _|_ & T /\\ _|_ =< T") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      val top = octDomain.top(n)
      val bottom = octDomain.bottom(n)
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
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      val top = octDomain.top(n)
      val bottom = octDomain.bottom(n)
      assert (!(top == bottom))
      assert (!(bottom == top))
      assert (!(top > top))
      assert (!(top < top))
      assert (!(bottom < bottom))
      assert (!(bottom > bottom))
    }
  }
}
