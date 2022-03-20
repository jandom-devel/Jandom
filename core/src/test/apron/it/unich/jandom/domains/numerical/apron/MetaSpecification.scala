/**
  * Copyright 2017 Tobia Tesan
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
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import spire.math.Rational

class MetaSpecification extends AnyPropSpec with ScalaCheckPropertyChecks {
  import it.unich.jandom.domains.numerical.Utils
  import Utils._
  import Rationals._
  import Rationals.arbRational

  /*
   * Checks on the sanity of test utils themselves
   */
  property("Check that GenGreaterRat consistently yields greater rationals") {
    forAll(arbitrary[Rational]) {
      r : Rational =>
      forAll(GenGreaterRat(r)) {
        gen =>
        assert(r < gen)
      }
    }
  }

  property("Check that GenSmallerRat consistently yields smaller rationals") {
    forAll(arbitrary[Rational]) {
      r : Rational =>
      forAll(GenSmallerRat(r)) {
        gen =>
        assert(r > gen)
      }
    }
  }

}
