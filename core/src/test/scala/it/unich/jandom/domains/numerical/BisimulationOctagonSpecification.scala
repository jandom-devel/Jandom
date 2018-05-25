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
import it.unich.jandom.domains.numerical.octagon.simple._
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import it.unich.jandom.utils.numberext.RationalExt

/**
 * This test checks that the "simple" octagon behaves like the
 * "optimized" octagon on a sequence of assignments and tests and that
 * the latter is overapproximated by boxes.
 */

// TODO: Extend to union, widening and intersection operations

class BisimulationOctagonSpecification extends PropSpec with PropertyChecks {
  import it.unich.jandom.domains.numerical.Utils.Ints._
  implicit val boxDomain : BoxRationalDomain = BoxRationalDomain()

  class SimpleOctagonDomain(boxDomain : BoxGenericDomain[RationalExt]) extends OctagonDomain(boxDomain) {
    implicit val okt : OctagonDomain = this
    override def top(dim : Int) : Property = new OctagonProperty(new SimpleOctagon(factory.top((OctagonDim(dim).toDBMDim))))(ifield, okt, box)
    override def bottom(dim : Int): Property = new OctagonProperty(new BottomOctagon(OctagonDim(dim)))(ifield, okt, box)
  }

  val simpleOctDomain = new SimpleOctagonDomain(boxDomain)
  val octDomain = OctagonDomain()


  property("Weak oct-box bisimilarity: oct-T.{ some ops }.toBox <= oct-T.toBox{ some ops }") {
    forAll(GenTinyPosInt.suchThat(_ > 1)) {
      n => {
        forAll(GenTinyPosInt.suchThat(_ > 1)) {
          l => {
            import Utils._
            import OpSequences._
            forAll(genSeq(n,l)) {
              opSeq => {
                val top = octDomain.top(n)
                val boxtop = boxDomain.top(n)
                assert(top.toBox <= boxtop)
                val oct = opSeq.foldLeft(top)((op, p) => applyOp(op)(p))
                val box = opSeq.foldLeft(boxtop)((op, p) => applyOp(op)(p))
                assert(oct.toBox <= box, opSeq + " + "  + oct + oct.constraints + oct.toBox + " <= " + box)
              }
            }
          }
        }
      }
    }
  }
}

package octagon.optimized {
  import it.unich.jandom.utils.numberext.IField

  class BisimulationOptimizedOctagonSpecification extends PropSpec with PropertyChecks {
    import it.unich.jandom.domains.numerical.Utils.Ints._
    implicit val boxDomain : BoxRationalDomain = BoxRationalDomain()

    class SimpleOctagonDomain(boxDomain : BoxGenericDomain[RationalExt]) extends OctagonDomain(boxDomain) {
      implicit val okt : OctagonDomain = this
      override def top(dim : Int) : Property = new OctagonProperty(new SimpleOctagon(factory.top((OctagonDim(dim).toDBMDim))))(ifield, okt, box)
      override def bottom(dim : Int): Property = new OctagonProperty(new BottomOctagon(OctagonDim(dim)))(ifield, okt, box)
    }

    val simpleOctDomain = new SimpleOctagonDomain(boxDomain)
    val octDomain = OctagonDomain()


    def compareOcts[N <: IField[N]] (opt : Octagon[N], simp : Octagon[N]) : Option[Int] = {
      opt match {
        case bot : BottomOctagon[N] => bot.tryCompareTo(simp)
        case opt_ : OptimizedOctagon[N] =>
          opt_.closedDbm match {
            case Some(us) => simp match {
              case _ : BottomOctagon[N] => Some(1) // They're bottom, we're greater
              case o : OptimizedOctagon[N] => o.closedDbm match {
                case Some(them) => us.tryCompareTo(them)
                case None => Some(1) // They're bottom, we're greater
              }
              //// Additional case for comparing with SimpleOctagons
              case s : simple.SimpleOctagon[N] => us.tryCompareTo(s.m)
              ////
              case _ => ??? // Not impl
            }
            case None => simp match {
              case _ : BottomOctagon[N] => Some(0) // They're bottom,
                                                   // we're also bottom
              case o : OptimizedOctagon[N] => o.closedDbm match {
                case Some(them) => Some(-1) // They're greater, we're
                                            // bottom
                case None => Some(0)        // Both bottom
              }
              //// Additional case for comparing with SimpleOctagons
              case s : simple.SimpleOctagon[N] => Some(-1) // They're a
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

    property("oct-simpleoct bisimilarity: oct-T.{ some ops } == simpleoct-T.{ some ops }") {
      forAll(GenTinyPosInt.suchThat(_ > 1)) {
        n => {
          forAll(GenTinyPosInt.suchThat(_ > 1)) {
            l => {
              import Utils._
              import OpSequences._
              forAll(genSeq(n,l)) {
                opSeq => {
                  val top = octDomain.top(n)
                  val simpletop = simpleOctDomain.top(n)
                  assert(compareOcts(top.o, simpletop.o) == Some(0))
                  val oct = opSeq.foldLeft(top)((op, p) => applyOp(op)(p))
                  val simpleoct = opSeq.foldLeft(simpletop)((op, p) => applyOp(op)(p))
                  assert(compareOcts(oct.o, simpleoct.o) == Some(0))
                  assert(oct.constraints == simpleoct.constraints)
                }
              }
            }
          }
        }
      }
    }
  }
}
