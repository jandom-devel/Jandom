package it.unich.jandom.domains.numerical

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

  object OpSequences {
    import Rationals._
    def genExactLf(n : Int) : Gen[LinearForm] = {
      assert(n >= 2)
      for {
        i <- Gen.choose(0, n - 1)
        j <- Gen.choose(0, n - 1)// .suchThat(_ != i)
        coeffi <- Gen.choose[Int](-1,1)
        coeffj <- Gen.choose[Int](-1,1)
        c <- GenRational
      } yield {
        val arr = Array.fill(n)(0).updated(i, coeffi).updated(j, coeffj)
        LinearForm(arr : _*)
      }
    }

    def gen1ExactLf(n : Int) : Gen[LinearForm] = {
      assert(n >= 2)
      for {
        i <- Gen.choose(0, n - 1)
        coeffi <- Gen.choose[Int](-1,1)
        c <- GenRational
      } yield {
        val arr = Array.fill(n)(0).updated(i, coeffi)
        LinearForm(arr : _*)
      }
    }

    def genAnyLf(n : Int) = Gen.frequency((5, genExactLf(n)), (1, LinearForms.GenLf(n)))

    trait Op
    case class LinearAssignment(i : Int, l : LinearForm) extends Op
    case class LinearInequality(l : LinearForm) extends Op

    def genAssign[P <: NumericalProperty[P]](n : Int) : Gen[Op] = {
      for {
        lf <- genAnyLf(n) // gen1ExactLf(n)// .suchThat(_.homcoeffs.filter(_ != 0).size <= 1)
        i  <- Gen.choose(0, n - 1)
      }
      yield {
        LinearAssignment(i, lf)
      }
    }

    def genIneq[P <: NumericalProperty[P]](n : Int) : Gen[Op] = {
      for {
        lf <- genAnyLf(n) //ExactLf(n)
        i  <- Gen.choose(0, n - 1)
      }
      yield {
        LinearInequality(lf)
      }
    }

    def genSeq(n : Int, length : Int) : Gen[Seq[Op]] = {
      def genAnyOp : Gen[Op] = Gen.frequency((1, genAssign(n)), (1, genIneq(n)))
      Gen.containerOfN[Seq, Op](length, genAnyOp)
    }

    def applyOp[P <: NumericalProperty[P]](p : P)(op : Op) : P = {
      op match {
        case LinearAssignment(i, lf) =>
          p.linearAssignment(i, lf)
        case LinearInequality(lf) =>
          p.linearInequality(lf)
        case _ => ???
      }
    }
  }

  object Halfplanes {
    import org.scalacheck.Gen.sequence

    /////////////////////////////////
    sealed trait Halfplane
    // vi >= c
    case class R(i : Int, c : Rational) extends Halfplane
    // vi <= c
    case class R_(i : Int, c : Rational) extends Halfplane
    // vi + vj >= c
    case class S1(i : Int, j : Int, c : Rational) extends Halfplane
    // vi + vj <= c
    case class S2(i : Int, j : Int, c : Rational) extends Halfplane
    // vi - vj >= c
    case class S3(i : Int, j : Int, c : Rational) extends Halfplane
    // vi - vj <= c
    case class S4(i : Int, j : Int, c : Rational) extends Halfplane

    def GenOctagonHalfplanes(point : Int => Gen[List[Rational]], smaller : Rational => Gen[Rational], greater : Rational => Gen[Rational])(n : Int) : Gen[List[Halfplane]] = {
      def GenR (t : (Int, Rational)) = for {
        r <- smaller(t._2)
      } yield R(t._1, r)

      def GenR_ (t : (Int, Rational)) = for {
        r <- greater(t._2)
      } yield R_(t._1, r)

      def GenS1 (t : (Int, Int, Rational, Rational)) = for {
        r <- smaller(t._3 + t._4)
      } yield S1(t._1, t._2, r)

      def GenS2 (t : (Int, Int, Rational, Rational)) = for {
        r <- greater(t._3 + t._4)
      } yield S2(t._1, t._2, r)

      def GenS3 (t : (Int, Int, Rational, Rational)) = for {
        r <- smaller((t._3 - t._4))
      } yield S3(t._1, t._2, r)

      def GenS4 (t : (Int, Int, Rational, Rational)) = for {
        r <- greater((t._3 - t._4))
      } yield S4(t._1, t._2, r)

      for {
        p <- point(n).map(x => (0 until x.length).zip(x).toList)
        r <- sequence[List[R], R](p.map(GenR(_)))
        r_ <- sequence[List[R_], R_](p.map(GenR_(_)))
        s1 <- sequence[List[S1], S1](p.flatMap(i => p.map(j => GenS1((i._1, j._1, i._2, j._2))))).map(_.filter(x => x.i > x.j))
        s2 <- sequence[List[S2], S2](p.flatMap(i => p.map(j => GenS2((i._1, j._1, i._2, j._2))))).map(_.filter(x => x.i > x.j))
        s3 <- sequence[List[S3], S3](p.flatMap(i => p.map(j => GenS3((i._1, j._1, i._2, j._2))))).map(_.filter(x => x.i > x.j))
        s4 <- sequence[List[S4], S4](p.flatMap(i => p.map(j => GenS4((i._1, j._1, i._2, j._2))))).map(_.filter(x => x.i > x.j))
      } yield {
        assert(p.length == n)
        assert(r.length == n)
        r ++ r_ ++ s1 ++ s2 ++ s3 ++ s4
      }
    }

    def octagonFromHalfPlanes(dom : NumericalDomain)(n : Int, h : Option[List[Halfplane]]) = {
      h match {
        case None => dom.bottom(n) //BottomOctagon(OctagonDim(n))
        case Some(l) =>
      l.foldLeft(dom.top(n))(
        (z,f) => f match {
          case R(i,c) => {
            // lf == hom >= c
            // hom >= c
            val coeffs : List[Rational] = (List.fill[Rational](n + 1)(0)).updated(0, c).updated(i+1, Rational(1))
            val lf = LinearForm(coeffs : _*)
            z.linearInequality(lf)
          }
          case R_(i,c) =>  {
            // lf == hom >= c
            // hom <= c => -hom >= -c
            val coeffs : List[Rational] = (List.fill[Rational](n + 1)(0)).updated(0, -c).updated(i+1, Rational(-1))
            val lf = LinearForm(coeffs : _*)
            z.linearInequality(lf)
          }
          case S1 (i, j, c) => {
            // i + j >= c
            val coeffs : List[Rational] = (List.fill[Rational](n + 1)(0)).updated(0, c).updated(i+1, Rational(1)).updated(j+1, Rational(1))
            val lf = LinearForm(coeffs : _*)
            z.linearInequality(lf)

          }
          case S2 (i, j, c) => {
            // i + j <= c ==> - i - j >= -c
            val coeffs : List[Rational] = (List.fill[Rational](n + 1)(0)).updated(0, -c).updated(i+1, Rational(-1)).updated(j+1, Rational(-1))
            val lf = LinearForm(coeffs : _*)
            z.linearInequality(lf)

          }
          case S3 (i, j, c) => {
            // i - j >= c
            val coeffs : List[Rational] = (List.fill[Rational](n + 1)(0)).updated(0, c).updated(i+1, Rational(1)).updated(j+1, Rational(-1))
            val lf = LinearForm(coeffs : _*)
            z.linearInequality(lf)
          }
          case S4 (i, j, c) => {
            // i - j <= c
            val coeffs : List[Rational] = (List.fill[Rational](n + 1)(0)).updated(0, -c).updated(i+1, Rational(-1)).updated(j+1, Rational(1))
            val lf = LinearForm(coeffs : _*)
            z.linearInequality(lf)
          }
        }
      )}
    }

    import scala.reflect.ClassTag
    import it.unich.jandom.utils.numberext.RationalExt
    implicit val ctag = ClassTag
    implicit val ifield = RationalExt
    import it.unich.jandom.domains.numerical.octagon._

    import  it.unich.jandom.utils.dbm._
    val factory = new ArrayDBMFactory[RationalExt]
    def dbmFromHalfPlanes(n : Int, l : List[Halfplane]) : DBM[RationalExt] = {
      val vm : DBM[RationalExt] = factory.fromFun(OctagonDim(n).toDBMDim, idx => RationalExt.PositiveInfinity)
      l.foldLeft(vm)(
        (z,f) => f match {
          case R(i,c) => {
            // lower bound on vi, ie vi >= c
            (z).updated(DBMIdx(Var(i).negForm.i, Var(i).posForm.i))(-2*c)
          }
          case R_(i,c) =>  {
            // z
            (z).updated(DBMIdx(Var(i).posForm.i, Var(i).negForm.i))(2*c)
          }
          case S1 (i, j, c) => {
            // s1 is lower bound on i+j
            (z).updated(DBMIdx(Var(j).posForm.i, Var(i).negForm.i))(-c)
              .updated(DBMIdx(Var(i).negForm.i, Var(j).posForm.i))(-c)
            // i + j >= c
            // => -i -j <= -c
          }
          case S2 (i, j, c) => {
            // s2 is upper bound on i + 1
            (z).updated(DBMIdx(Var(i).posForm.i, Var(j).negForm.i))(c)
              .updated(DBMIdx(Var(j).negForm.i, Var(i).posForm.i))(c)
            // i + j <= c ==> - i - j >= -c
          }
          case S3 (i, j, c) => {
            // s2 is loew bound on i - j, i.e.
            // i - j >= c => -i + j <= - c => j - i <= -c
            (z).updated(DBMIdx(Var(j).posForm.i, Var(i).posForm.i))(-c)
              .updated(DBMIdx(Var(i).negForm.i, Var(j).negForm.i))(-c)
          }
          case S4 (i, j, c) => {
            // S4 is upper bound on i - j
            // i - j <= c
            (z).updated(DBMIdx(Var(i).posForm.i, Var(j).posForm.i))(c)
              .updated(DBMIdx(Var(j).negForm.i, Var(i).negForm.i))(c)
            // i - j <= c
          }
        }
      )
    }

    import Rationals._

    def GenOctagonHalfplanesRat = (n : Int) => Gen.option(GenOctagonHalfplanes((n : Int) => Gen.containerOfN[List, Rational](n, GenRational),
        r => Rationals.GenSmallerRat(r),
        r => Rationals.GenGreaterRat(r)
      )((n)))


  }
}
