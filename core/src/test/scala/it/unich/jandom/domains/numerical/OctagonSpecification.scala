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
import it.unich.jandom.utils.dbm._
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import it.unich.jandom.utils.numberext.RationalExt
import spire.math.Rational
import it.unich.jandom.domains.numerical.Utils.Ints._


abstract class OctagonSpecification extends AnyPropSpec with ScalaCheckPropertyChecks {
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

  // ///////////////////////////////////////////////////////
  // // Basic ordering properties of Octagon octagons
  // ///////////////////////////////////////////////////////


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

  // /////////////////////////////////////////
  // // Ordering/numerical properties
  // ////////////////////////////////////////

  property ("Union of [C1,C1], [C2, C2] == [C1, C2] w/C1 <= C2") {
    forAll(Utils.Rationals.GenRationalInterval) {
      (c : (Rational, Rational)) => {
        val a = octDomain.top(1)
        val b1 = a.linearAssignment(0, LinearForm.c(c._1))
        val b2 = a.linearAssignment(0, LinearForm.c(c._2))
        val union = b1 union b2
        assert(c._1 <= c._2)
        assert(union.toBox.isEmpty == false &
         union.toBox.high.head == c._2 &
         union.toBox.low.head == c._1)
      }
    }
  }

  property ("Intersection of [C1,C1], [C2, C2] == _|_ w/C1 < C2") {
    forAll(Utils.Rationals.GenRationalInterval.suchThat(pair => pair._1 < pair._2)) {
      (c : (Rational, Rational)) => {
        val a = octDomain.top(1)
        val b1 = a.linearAssignment(0, LinearForm.c(c._1))
        val b2 = a.linearAssignment(0, LinearForm.c(c._2))
        val intersection = b1 intersection b2
        assert(c._1 <= c._2)
        assert(intersection.isBottom == true &
         intersection.toBox.isEmpty == true)
      }
    }
  }

  property ("[C1,C2] <= [C3<C1, C4>C2]") {
    forAll(Utils.Rationals.GenRationalInterval) {
      (c: (Rational, Rational)) =>
    forAll(Utils.Rationals.GenRationalInterval) {
          (d: (Rational, Rational)) => {
            val c1 = c._1
            val c2 = c._2
            assert(c._1 <= c._2)
            assert(d._1 <= d._2)
            val posC = (d._2 - d._1) // is certainly non-neg
            assert(posC >= 0)
            val c3 = c1 - posC
            val c4 = c2 + posC
            assert(c3 <= c1 & c1 <= c4 & c4 >= c2)
            val a = octDomain.top(1)
            val b1 = a.linearAssignment(0, LinearForm.c(c1))
            val b2 = a.linearAssignment(0, LinearForm.c(c2))
            val b3 = a.linearAssignment(0, LinearForm.c(c3))
            val b4 = a.linearAssignment(0, LinearForm.c(c4))
            val i1 = b1 union b2 // i1 = [c1,c2]
            val i2 = b3 union b4 // i2 = [c3,c4]
            assert(i1 <= i2)
          }
        }
    }
  }

  ///////////////////////////////////////////////////////
  // Properties of Octagon.assignments
  ///////////////////////////////////////////////////////

  property(".assign_vj0_gets_c works as intended") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        _j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val j0 = _j0 + 1
          import scala.reflect.ClassTag
          implicit val rext = RationalExt
          implicit val tag = implicitly[ClassTag[RationalExt]]
          implicit val afac = new ArrayDBMFactory()(tag, rext)
          implicit val bsc = new MineFloydWarshall[RationalExt]()(rext, afac)
          def topoct = new optimized.PedanticOctagon(afac.top(OctagonDim(n).toDBMDim))
          // val top = octDomain.top(n)
          val updated = topoct.assign_vj0_gets_c(Var(j0), c)
          assert(!updated.isBottom)
          // Vi <= c is represented as Vi_pos - Vi_neg <= 2c, Mine' 2006 p 8
          assert(updated.get_ineq_vi_minus_vj_leq_c(Var(j0).posForm, Var(j0).negForm).get == RationalExt(2 * c))
          // Vi >= c is represented as Vi_neg - Vi_pos <= -2c, Mine' 2006 p 8
          assert(updated.get_ineq_vi_minus_vj_leq_c(Var(j0).negForm, Var(j0).posForm).get == RationalExt(-2 * c))
        }
      }
    }
  }

  ///////////////////////////////////////////////////////
  // Numerical properties of OctagonProperty
  ///////////////////////////////////////////////////////

  property("T.{x = x + c} == T, .constraints = emptyset") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val c001 = Array.fill(n + 1)(0).updated(j0 + 1, 1).updated(0, c)
          val assigned  = top.linearAssignment(j0, LinearForm(c001))
          val boxassigned  = boxDomain.top(n).linearAssignment(j0, LinearForm(c001))
          assert(!assigned.isBottom)
          assert (assigned <= top)
          assert (assigned == top)
          assert (assigned.constraints.size == 0)
          assert(assigned.toBox <= boxassigned)
        }
      }
    }
  }

  property("T.{x = -x} == T, .constraints = emptyset") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        val top = octDomain.top(n)
        assert (top.constraints.size == 0)
        val _0m10 = Array.fill(n + 1)(0).updated(j0 + 1, -1)
        val assigned  = top.linearAssignment(j0, LinearForm(_0m10))
        assert(!assigned.isBottom)
        assert (assigned <= top)
        assert (assigned == top)
        assert (assigned.isTop)
        assert (assigned.constraints.size == 0)
      }
    }
  }

  property("T.{x = c} != T") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val c000 = zero.updated(0, c)
          val assigned  = top.linearAssignment(j0, LinearForm(c000))
          assert(!assigned.isBottom)
          assert (!assigned.isTop)
          assert (assigned != top)
          assert (assigned <= top)
        }
      }
    }
  }

  property("T.{x = c}.constraints == Seq(x - c <= 0, -x + c <= 0)") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val c000 = zero.updated(0, c)
          val assigned  = top.linearAssignment(j0, LinearForm(c000))
          val constraints = assigned.constraints
          assert(constraints.size == 2)
          val c010 = zero.updated(j0 + 1, 1).updated(0, -c)   // v - c <= 0
          val c0m10 = zero.updated(j0 + 1, -1).updated(0, c)  // -v + c <= 0
          assert (constraints.contains(LinearForm(c010)))
          assert (constraints.contains(LinearForm(c0m10)))
        }
      }
    }
  }

  property("T.{x = c}.{x = -x} constraints == Seq(x - (-c) <= 0, -x + (-c) <= 0)") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val c000 = zero.updated(0, c)
          val _00m10 = zero.updated(j0 + 1, -1)
          val assigned  = top
            .linearAssignment(j0, LinearForm(c000))
            .linearAssignment(j0, LinearForm(_00m10))
          val constraints = assigned.constraints
          assert(constraints.size == 2)
          val mc010 = zero.updated(j0 + 1, 1).updated(0, -(-c))
          val mc0m10 = zero.updated(j0 + 1, -1).updated(0, (-c))
          assert (constraints.contains(LinearForm(mc010)))
          assert (constraints.contains(LinearForm(mc0m10)))
        }
      }
    }
  }

  property("T.{x = c}.{x = -x}.{x = -x} == T.{x = c}") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val c000 = zero.updated(0, c)
          val _00m10 = zero.updated(j0 + 1, -1)
          val assigned  = top
            .linearAssignment(j0, LinearForm(c000))
          val assigned2 = assigned
            .linearAssignment(j0, LinearForm(_00m10))
            .linearAssignment(j0, LinearForm(_00m10))
          assert(assigned == assigned2)
          assert(assigned.constraints == assigned2.constraints)
        }
      }
    }
  }

  property("T.{x = c}.{x = x + d} constraints == Seq(x - (c + d) <= 0, -x + (c + d) <= 0)") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          forAll (GenMediumInt){
            d : Int
            =>
            val top = octDomain.top(n)
            assert (top.constraints.size == 0)
            val zero = Array.fill(n + 1)(0)
            assert(zero.size == n + 1)
            val c000 = zero.updated(0, c)
            val d000 = zero.updated(j0 + 1, 1).updated(0, d)
            val assigned  = top
              .linearAssignment(j0, LinearForm(c000))
              .linearAssignment(j0, LinearForm(d000))
            val constraints = assigned.constraints
            assert(constraints.size == 2)
            val c010 = zero.updated(j0 + 1, 1).updated(0, -(c + d))  // v - (c + d) <= 0
            val c0m10 = zero.updated(j0 + 1, -1).updated(0, (c + d)) // -v + (c + d) <= 0
            assert (constraints.contains(LinearForm(c010)))
            assert (constraints.contains(LinearForm(c0m10)))
          }
        }
      }
    }
  }


  property("T.{x = c}.(x = x + 1}, .constraints == {x <= c + 1, -x <= -c - 1}") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val c000 = zero.updated(0, c)
          val assigned  = top.linearAssignment(j0, LinearForm(c000))
          assert(!assigned.isBottom)
          assert (assigned != top)
          assert (assigned <= top)
          assert (assigned.constraints.size == 2)
          val c010 = zero.updated(j0 + 1, 1).updated(0, -c)  // v - c <= 0
          val c0m10 = zero.updated(j0 + 1, -1).updated(0, c) // -v + c <= 0
          assert (assigned.constraints.contains(LinearForm(c010)))
          assert (assigned.constraints.contains(LinearForm(c0m10)))
        }
      }
    }
  }

  property("T.{x <= c} != T") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val c001 = zero.updated(0, c).updated(j0 + 1, 1)
          val assigned  = top.linearInequality(LinearForm(c001))
          assert (assigned != top)
          assert (assigned <= top)
          assert (assigned.constraints.size == 1)
        }
      }
    }
  }

  property("T.{x <- - c + 1}{x + c <= 0} == _|_") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val d = - c + 1
          /*
           * v <- - c + 1
           * if (v + c <= 0) == (1 -c + c <= 0) then
           *     // _|_
           * end
           */
          val d001 = zero.updated(0, d).updated(j0 + 1, 1)
          val c000 = zero.updated(0, c).updated(j0 + 1, 0)
          val assigned  = top.linearAssignment(j0, LinearForm(c000))
          val inequal  = assigned.linearInequality(LinearForm(d001))
          assert (inequal != top)
          assert (assigned <= top)
          assert (inequal.isBottom)
          assert (inequal == octDomain.bottom(n))
        }
      }
    }
  }

  property("T.{x <- c}{x - c <= 0}.constraints { x - c <= 0, -x + c <= 0}") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val assigned  = top.linearAssignment(j0, LinearForm.c(c))
          val mc001 = zero.updated(0, -c).updated(j0 + 1, 1) // x -c <= 0
          val inequal  = assigned.linearInequality(LinearForm(mc001))
          assert (inequal != top)
          assert (inequal <= top)
          assert (!inequal.isBottom)
          assert (inequal != octDomain.bottom(n))
          assert (inequal.constraints.size == 2)
          assert (inequal.constraints.contains(LinearForm(zero.updated(j0 + 1, 1).updated(0, -c))))
          assert (inequal.constraints.contains(LinearForm(zero.updated(j0 + 1, -1).updated(0, +c))))
        }
      }
    }
  }

  property("T.{-x + c <= 0}{x - c <= 0}.constraints { x - c <= 0, -x + c <= 0}") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt.suchThat(_ != 0)){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val mc001 = zero.updated(0, -c).updated(j0 + 1, 1)  // x -c <= 0
          val c00m1 = zero.updated(0,  c).updated(j0 + 1, -1) // -x + c <= 0
          val inequal = top.linearInequality(LinearForm(mc001)).linearInequality(LinearForm(c00m1))
          assert (inequal != top)
          assert (inequal <= top)
          assert (!inequal.isBottom)
          assert (inequal != octDomain.bottom(n))
          assert (inequal.constraints.size == 2)
          assert (inequal.constraints.contains(LinearForm(mc001)))
          assert (inequal.constraints.contains(LinearForm(c00m1)))
        }
      }
    }
  }


  property("T.{x + y + c <= 0}.constraints = { x + y + c <= 0}") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (Gen.choose(0, n - 1)){
          i0 : Int
          =>
          forAll (GenMediumInt.suchThat(_ != 0)){
            c : Int
            =>
            val top = octDomain.top(n)
            assert (top.constraints.size == 0)
            val zero = Array.fill(n + 1)(0)
            assert(zero.size == n + 1)
            val c011 = zero.updated(0, -c).updated(j0 + 1, 1).updated(i0 + 1, 1)
            val inequal = top.linearInequality(LinearForm(c011))
            assert (inequal != top)
            assert (inequal <= top)
            assert (!inequal.isBottom)
            assert (inequal != octDomain.bottom(n))
            assert (inequal.constraints.size == 1)
            assert (inequal.constraints.contains(LinearForm(c011)))
          }
        }
      }
    }
  }


  property("T.{x - y + c <= 0}.constraints = { x - y + c <= 0}") {
    forAll (GenTinyPosInt.suchThat(_ > 1)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (Gen.choose(0, n - 1).suchThat(_ != j0)){
          i0 : Int
          =>
          forAll (GenMediumInt.suchThat(_ != 0)){
            c : Int
            =>
            val top = octDomain.top(n)
            assert (top.constraints.size == 0)
            val zero = Array.fill(n + 1)(0)
            assert(zero.size == n + 1)
            val c011 = zero.updated(0, -c).updated(j0 + 1, 1).updated(i0 + 1, -1)
            val inequal = top.linearInequality(LinearForm(c011))
            assert (inequal != top)
            assert (inequal <= top)
            assert (!inequal.isBottom)
            assert (inequal != octDomain.bottom(n))
            assert (inequal.constraints.size == 1)
            assert (inequal.constraints.contains(LinearForm(c011)))
          }
        }
      }
    }
  }

  property("T.{-x - y + c <= 0}.constraints = {-x - y + c <= 0}") {
    forAll (GenTinyPosInt.suchThat(_ > 1)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (Gen.choose(0, n - 1).suchThat(_ != j0)){
          i0 : Int
          =>
          forAll (GenMediumInt.suchThat(_ != 0)){
            c : Int
            =>
            val top = octDomain.top(n)
            assert (top.constraints.size == 0)
            val zero = Array.fill(n + 1)(0)
            assert(zero.size == n + 1)
            val c011 = zero.updated(0, +c).updated(j0 + 1, -1).updated(i0 + 1, -1)
            val inequal = top.linearInequality(LinearForm(c011))
            assert (inequal != top)
            assert (inequal <= top)
            assert (!inequal.isBottom)
            assert (inequal != octDomain.bottom(n))
            assert (inequal.constraints.size == 1)
            assert (inequal.constraints.contains(LinearForm(c011)))
          }
        }
      }
    }
  }

  property("T.{x <- c}.{x - y + 0 <= 0} constraints include x <= y -> c <= y -> -y + c <= 0") {
    forAll (GenTinyPosInt.suchThat(_ > 1)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (Gen.choose(0, n - 1).suchThat(_ != j0)){
          i0 : Int
          =>
          forAll (GenMediumInt.suchThat(_ != 0)){
            c : Int
            =>
            val top = octDomain.top(n)
            assert (top.constraints.size == 0)
            val zero = Array.fill(n + 1)(0)
            assert(zero.size == n + 1)
            val c000 = zero.updated(0, c)
            val _001m1 = zero.updated(0, 0).updated(j0 + 1, 1).updated(i0 + 1, -1)
            val inequal_1 = top.linearAssignment(j0, LinearForm(c000))
            val inequal_3 = inequal_1.linearInequality(LinearForm(_001m1))
            val c00m1 = zero.updated(0, c).updated(i0 + 1, -1)
            assert (inequal_3.constraints.contains(LinearForm(c00m1)))
          }
        }
      }
    }
  }


  property("T.{x <- c}.{y <- x} implies  y = c, constraints include y - c <= 0, - y + c >= 0") {
    forAll (GenTinyPosInt.suchThat(_ > 1)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (Gen.choose(0, n - 1).suchThat(_ != j0)){
          i0 : Int
          =>
          forAll (GenMediumInt.suchThat(_ != 0)){
            c : Int
            =>
            val top = octDomain.top(n)
            assert (top.constraints.size == 0)
            val zero = Array.fill(n + 1)(0)
            assert(zero.size == n + 1)
            val c000 = zero.updated(0, c)
            val _0001 = zero.updated(0, 0).updated(j0 + 1, 1)
            val inequal = top.
              linearAssignment(j0, LinearForm(c000))
              .linearAssignment(i0, LinearForm(_0001))
            val c000m1 = zero.updated(0, c).updated(i0 + 1, -1)
            val mc0001 = zero.updated(0, -c).updated(i0 + 1, 1)
            assert (inequal != top)
            assert (inequal <= top)
            assert (!inequal.isBottom)
            assert (inequal != octDomain.bottom(n))
            assert (inequal.constraints.contains(LinearForm(c000m1)))
            assert (inequal.constraints.contains(LinearForm(mc0001)))
          }
        }
      }
    }
  }

  property("T.{x <- c}{-x + c <= 0}{x - c <= 0}.constraints { x - c <= 0, -x + c <= 0}") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt.suchThat(_ != 0)){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val mc001 = zero.updated(0, -c).updated(j0 + 1, 1)  // x -c <= 0
          val c00m1 = zero.updated(0,  c).updated(j0 + 1, -1) // -x + c <= 0
          val inequal = top.linearAssignment(j0, LinearForm.c(c))
            .linearInequality(LinearForm(mc001))
            .linearInequality(LinearForm(c00m1))
          assert (inequal != top)
          assert (inequal <= top)
          assert (!inequal.isBottom)
          assert (inequal != octDomain.bottom(n))
          assert (inequal.constraints.size == 2)
          assert (inequal.constraints.contains(LinearForm(mc001)))
          assert (inequal.constraints.contains(LinearForm(c00m1)))
        }
      }
    }
  }

  //////////////////////////////////

  property("T.{x <= c}.constraints == {x <= c}") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val c001 = zero.updated(0, c).updated(j0 + 1, 1) // c + 1 <= 0
          val lf = LinearForm(c001)
          val assigned  = top.linearInequality(lf)
          assert (assigned != top)
          assert (assigned <= top)
          assert (assigned.constraints.size == 1)
          assert(top.dimension == assigned.dimension & top.dimension == n)
          assert (assigned.constraints.head.padded(n + 1) == lf)
        }
      }
    }
  }

  property("T.{x - y <= c}.constraints == {x - y <= c}") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (Gen.choose(0, n - 1)){
          i0 : Int
          =>
          forAll (GenMediumInt){
            c : Int
            =>
            val top = octDomain.top(n)
            assert (top.constraints.size == 0)
            val zero = Array.fill(n + 1)(0)
            assert(zero.size == n + 1)
            val c001 = zero.updated(0, c).updated(j0 + 1, 1).updated(i0 + 1, - 1)
            val lf = LinearForm(c001)
            val assigned  = top.linearInequality(lf)
            assert (assigned != top)
            assert (assigned <= top)
            assert (assigned.constraints.size == 1)
            assert(top.dimension == assigned.dimension & top.dimension == n)
            assert (assigned.constraints.head.padded(n + 1) == lf)
          }
        }
      }
    }
  }

  property("T.{x <- c}{x + c <= 0} == _|_") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val d = - c + 1
          /*
           * v <- - c + 1
           * if (v + c <= 0) == (1 -c + c <= 0) then
           *     // _|_
           * end
           */
          val d001 = zero.updated(0, d).updated(j0 + 1, 1)
          val c000 = zero.updated(0, c).updated(j0 + 1, 0)
          val assigned  = top.linearAssignment(j0, LinearForm(c000))
          val inequal  = assigned.linearInequality(LinearForm(d001))
          assert (inequal != top)
          assert (assigned <= top)
          assert (inequal.isBottom)
          assert (inequal == octDomain.bottom(n))
        }
      }
    }
  }

  property("T.{x = c} <= T") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      val top = octDomain.top(n)
      val bottom = octDomain.bottom(n)
      assert (top.union(top) == top)
      assert (bottom.union(top) == top)
    }
  }


  property("T.{x <- c}{x <- -x} == T.{x <- -c}") {
    forAll (GenTinyPosInt.suchThat(_ > 0)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val mc000 = zero.updated(0, -c)
          val c000 = zero.updated(0, c)
          val _00m1 = zero.updated(0, 0).updated(j0 + 1, -1)
          val x_gets_c  = top.linearAssignment(j0, LinearForm(c000))
          val x_gets_minus_c_then_gets_minus_x  =
            top.linearAssignment(j0, LinearForm(mc000))
              .linearAssignment(j0, LinearForm(_00m1))
          assert (x_gets_c == x_gets_minus_c_then_gets_minus_x)
          assert (x_gets_c <= x_gets_minus_c_then_gets_minus_x)
          assert (x_gets_c >= x_gets_minus_c_then_gets_minus_x)
        }
      }
    }
  }

  property("T.{x <- c}{y <- -x}{x <- -x} == T.{y <- -c}{x <- y}") {
    forAll (GenTinyPosInt.suchThat(_ > 1)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (Gen.choose(0, n - 1).suchThat(_ != j0)){
          i0 : Int
          =>
          forAll (GenMediumInt){
            c : Int
            =>
            val top = octDomain.top(n)
            assert (top.constraints.size == 0)
            val zero = Array.fill(n + 1)(0)
            assert(zero.size == n + 1)
            val mc000 = zero.updated(0, -c)
            val c000 = zero.updated(0, c)
            val _00m1 = zero.updated(0, 0).updated(j0 + 1, -1)
            val _010 = zero.updated(0, 0).updated(i0 + 1, 1)
            val y_gets_c_then_x_gets_y  =
              top.linearAssignment(i0, LinearForm(c000))//  i<- c
                .linearAssignment(j0, LinearForm(_010)) //  j<-+i
            val x_gets_minus_c_then_y_gets_minus_x_then_x_gets_minus_x  =
              top.linearAssignment(j0, LinearForm(mc000)) // j <- -c
                .linearAssignment(i0, LinearForm(_00m1))  // i <- -j
                .linearAssignment(j0, LinearForm(_00m1))  // j <- -j
            assert (y_gets_c_then_x_gets_y == x_gets_minus_c_then_y_gets_minus_x_then_x_gets_minus_x)
            assert (y_gets_c_then_x_gets_y <= x_gets_minus_c_then_y_gets_minus_x_then_x_gets_minus_x)
            assert (y_gets_c_then_x_gets_y >= x_gets_minus_c_then_y_gets_minus_x_then_x_gets_minus_x)
          }
        }
      }
    }
  }

  property("T.{x <- c} <= T.{x <= c + 1}{-x <= -c + 1}") {
    forAll (GenTinyPosInt.suchThat(_ > 1)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          val bottom = octDomain.bottom(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val c000 = zero.updated(0, c)
          val mcm0m10 = zero.updated(j0 + 1, 1).updated(0, - c - 1)  // x - c - 1 <= 0
          val cm0m10 = zero.updated(j0 + 1, -1).updated(0, + c - 1)  // -x + c - 1 <= 0
          val assigned = top.linearAssignment(j0, LinearForm(c000)) // j0 <- c
          val inequal = top.linearInequality(LinearForm(mcm0m10))
            .linearInequality(LinearForm(cm0m10))
          assert (assigned <= inequal)
          assert (assigned != inequal)
          assert (!(assigned >= inequal)) // fails
          assert (!(assigned >= top))
          assert (bottom <= inequal)
        }
      }
    }
  }

  property("T.{x <- c} !<= T.{x <= c - 1}{-x <= -c - 1} == _|_") {
                                         // x >= c + 1
    forAll (GenTinyPosInt.suchThat(_ > 1)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (GenMediumInt){
          c : Int
          =>
          val top = octDomain.top(n)
          val bottom = octDomain.bottom(n)
          assert (top.constraints.size == 0)
          val zero = Array.fill(n + 1)(0)
          assert(zero.size == n + 1)
          val c000 = zero.updated(0, c)
          val mcm0m10 = zero.updated(j0 + 1, 1).updated(0, - c + 1)  // x - c + 1 <= 0
          val cm0m10 = zero.updated(j0 + 1, -1).updated(0, + c + 1)  // -x + c + 1 <= 0
          val assigned = top.linearAssignment(j0, LinearForm(c000)) // j0 <- c
          val inequal = top.linearInequality(LinearForm(mcm0m10))
            .linearInequality(LinearForm(cm0m10))
          assert (!(assigned <= inequal))
          // assert(false, inequal.o.asInstanceOf[LazyOctagon[_]].dbm)
          assert (bottom == inequal)
        }
      }
    }
  }


  property("T.{x <- c} !<= T.{y <- c} & T.{x <- c} !<= T.{y <- c} & T.{x <- c} != T.{y <- c}") {
    forAll (GenTinyPosInt.suchThat(_ > 1)){
      n : Int
      =>
      forAll (Gen.choose(0, n - 1)){
        j0 : Int
        =>
        forAll (Gen.choose(0, n - 1).suchThat(_ != j0)) {
          i0 : Int =>
          forAll (GenMediumInt) {
            c : Int
            =>
            val top = octDomain.top(n)
            assert (top.constraints.size == 0)
            val zero = Array.fill(n + 1)(0)
            assert(zero.size == n + 1)
            val c000 = zero.updated(0, c)
            val ass1 = top.linearAssignment(j0, LinearForm(c000)) // j0 <- c
            val ass2 = top.linearAssignment(i0, LinearForm(c000)) // i0 <- c
            assert (ass1 != ass2)
            assert (!(ass1 <= ass2))
            assert (!(ass1 >= ass2))
          }
        }
      }
    }
  }

  ///////////////////////////////////////////////////////
  // Widening and narrowing
  ///////////////////////////////////////////////////////

  property ("forall X, Y : AbstractOctagon, (X widening Y) >= X, Y (condition 1 of 2 for soundness of widening)") {
    forAll(GenTinyPosInt.suchThat(_ > 1)) {
      n =>
      forAll(GenTinyPosInt.suchThat(_ > 1)) {
        l =>
        import Utils._
        import OpSequences._
        forAll(genSeq(n,l)) {
          opSeq => {
            forAll(GenTinyPosInt.suchThat(_ > 1)) {
              l2 => {
                import Utils._
                import OpSequences._
                forAll(genSeq(n,l2)) {
                  opSeq2 => {
                    val top = octDomain.top(n)
                    val x = opSeq.foldLeft(top)((op, p) => applyOp(op)(p))
                    val y = opSeq2.foldLeft(top)((op, p) => applyOp(op)(p))
                    assert((x widening y) >= x)
                    assert((x widening y) >= y)
                  }
                }
              }
            }
          }
        }
      }
    }
  }


  // TODO: check somehow that widening stabilizes
  // TODO: test narrowing
}
