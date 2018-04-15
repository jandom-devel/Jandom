package it.unich.jandom.domains.numerical
import it.unich.jandom.domains.numerical._

import org.scalacheck.Arbitrary
import org.scalacheck.Gen.Choose
import org.scalacheck.Gen
import spire.math.Rational
import it.unich.jandom.utils.numberext.RationalExt

object Utils {
  val kTinyPosInt = 5
  val kTinyNegInt = -5
  val kMediumPosInt = 2^5
  val kMediumNegInt = -2^5
  val kLargePosInt = Int.MaxValue / (2^5)
  val kLargeNegInt = Int.MinValue / (2^5)
  val kLargestPosInt = Int.MaxValue
  val kLargerstNegInt = Int.MinValue

  object Ints {
    def GenTinyInt : Gen[Int] = Gen.choose(kTinyNegInt, kTinyPosInt)
    def GenTinyPosInt : Gen[Int] = Gen.choose(1, kTinyPosInt)
    def GenMediumInt : Gen[Int] = Gen.choose(kMediumNegInt, kMediumPosInt)
  }

  object Rationals {

    def GenRational: Gen[Rational] =
      for {
        n <- Gen.choose(kMediumNegInt, kMediumPosInt)
        d <- Gen.choose(kMediumNegInt, kMediumPosInt).suchThat(_ != 0)
      } yield {
        Rational(n, d)
      }

    def GenPosRational: Gen[Rational] =
      for {
        n <- Gen.choose(1, kMediumPosInt)
        d <- Gen.choose(1, kMediumPosInt)
      } yield {
        Rational(n, d)
      }


    def GenRationalInterval : Gen[(Rational, Rational)] = for {
      a <- GenRational
      b <- GenRational
    } yield if (a <= b)
      (a, b)
    else (b, a)

    implicit object chooseRat extends Choose[Rational] {
      def choose(min: Rational, max: Rational): Gen[Rational] = {
        assert(min.denominator > 0)
        assert(max.denominator > 0)
        assert(min <= max)
        val lcm = spire.math.lcm(min.denominator, max.denominator)
        // Careful with overflows here
        val commonden = lcm
        val minnum = min.numerator * (max.denominator / lcm)
        val maxnum = max.numerator * (min.denominator / lcm)
        assert(Rational(minnum, commonden) == min)
        assert(Rational(maxnum, commonden) == max)
        assert(minnum <= maxnum)
        Gen.choose[Long](minnum.toLong, maxnum.toLong).map(n => Rational(n, commonden))
      }
    }

    implicit def arbRational: Arbitrary[Rational] =
      Arbitrary {
        GenRational
      }


    def GenGreaterRat(r : Rational) = {
      val arbnum = r.numerator * kMediumPosInt + 1 //arbitrary
      val arbden = r.denominator * kMediumPosInt //arbitrary
      val lower = Rational(arbnum, arbden)
      assert(lower > r)
      Gen.choose[Rational](lower, (lower + kMediumPosInt))
    }

    def GenSmallerRat(r : Rational) = {
      val arbnum = r.numerator * kMediumPosInt - 1 //arbitrary
      val arbden = r.denominator * kMediumPosInt //arbitrary
      val upper = Rational(arbnum, arbden)
      assert(upper < r)
      Gen.choose[Rational]((upper - kMediumPosInt), upper)
    }
  }

  object RationalExts {
    def GenFiniteRationalExt = for (a <- Rationals.GenRational) yield RationalExt(a)
    def GenFiniteRationalExtOrInf(pInf: Int = 10, pMinusInf: Int = 0) : Gen[RationalExt] = {
      assert(pInf >= 0)
      assert(pInf <= 100)
      assert(pMinusInf >= 0)
      assert(pMinusInf <= 100)
      assert(pMinusInf + pInf <= 100)
      Gen.frequency(
        (pInf, Gen.const(RationalExt.PositiveInfinity)),
        (pMinusInf, Gen.const(RationalExt.NegativeInfinity)),
        (100 - pInf - pMinusInf, GenFiniteRationalExt)
      )
    }

    implicit def arbRationalExt : Arbitrary[RationalExt] = Arbitrary { GenFiniteRationalExtOrInf(5, 5) }

    def GenFiniteRationalExtInterval : Gen[(RationalExt, RationalExt)] = for {
      a <- GenFiniteRationalExt
      b <- GenFiniteRationalExt
    } yield if (a <= b)
      (a, b)
    else (b, a)

    def GenRationalExtInterval(pInf: Int = 5, pMinusInf: Int = 5) : Gen[(RationalExt, RationalExt)] = for {
      a <- GenFiniteRationalExtOrInf(pInf, pMinusInf)
      b <- GenFiniteRationalExtOrInf(pInf, pMinusInf)
    } yield if (a <= b)
      (a, b)
    else (b, a)
  }

  object LinearForms {
    def GenLfAnyDim : Gen[LinearForm] = for {
      n <- Gen.choose(1,kTinyPosInt)
      lf <- GenLf(n)
    } yield lf

    def GenLf(n:Int) : Gen[LinearForm] =
      for {
        coef <- Gen.containerOfN[Seq, Rational](n, Rationals.GenRational)
      } yield LinearForm(coef :_*)

    def GenExactLf(n: Int): Gen[LinearForm] = for
    {
      c <- Gen.choose(kMediumNegInt, kMediumPosInt)
      vi <- Gen.choose(1, n)
      ci <- Gen.oneOf(+1, -1)
      vj <- Gen.choose(1, n)
      cj <- Gen.oneOf(+1, -1) // TODO This can potentially overwrite vi, which is as intended since this yields the 1-coeff case, TODO: require different
    } yield new DenseLinearForm(
      Array.fill[Rational](n+1)(0)
        .updated(0, Rational(c))
        .updated(vi, Rational(ci))
        .updated(vj, Rational(cj))
        .toSeq
    )
    implicit def arbLinearForm: Arbitrary[LinearForm] =
      Arbitrary { GenLfAnyDim }

    def GenIntLfAnyDim : Gen[LinearForm] = for {
      n <- Gen.choose(1,kTinyPosInt)
      lf <- GenIntLf(n)
    } yield lf

    def GenIntLf(n:Int) : Gen[LinearForm] =
      for {
        coef <- Gen.containerOfN[Seq, Rational](n, Gen.choose[Int](kMediumNegInt, kMediumPosInt).map(Rational(_)))
      } yield LinearForm(coef :_*)
  }
}
