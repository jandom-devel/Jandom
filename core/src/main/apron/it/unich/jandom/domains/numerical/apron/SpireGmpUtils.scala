package it.unich.jandom.domains.numerical.apron
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.utils.numberext.RationalExt
import apron._
import gmp.Mpq
import spire.math.Rational

object SpireGmpUtils {
  def ratToMpq(x : Rational) : Mpq = {
    assert(x.denominatorAsLong != 0)
    val num = java.math.BigInteger.valueOf(x.numeratorAsLong)
    val den = java.math.BigInteger.valueOf(x.denominatorAsLong)
    assert(den != 0)
    new Mpq(num, den)
  }

  def scalarToRatExt(s : Scalar) : RationalExt = {
    if (s.isInfty != 0) {
      if (s.isInfty == -1) {
         RationalExt.NegativeInfinity
      } else if (s.isInfty == 1) {
         RationalExt.PositiveInfinity
      } else {
        ???
      }
    } else if (s.isZero) {
      RationalExt.zero
    } else {
      assert(s.isInfty == 0 & s.isZero == false)
      val mpq : Mpq = new Mpq(1,1)
      val res = s.toMpq(mpq, 0) // has side effects on 1st arg
      assert(res == 0)
      assert(mpq.getDen.bigIntegerValue != 0)
      RationalExt(mpqToRat(mpq))
    }
  }

  def ratExtToScalar(r : RationalExt) : Scalar = {
    if (r.isPosInfinity) {
      val s = Scalar.create()
      s.setInfty(1)
      s
    }
    else if (r.isNegInfinity) {
      val s = Scalar.create()
      s.setInfty(-1)
      s
    } else if (r.isZero) {
      val s = Scalar.create()
      s.set(0)
      s
    } else {
      new MpqScalar(ratToMpq(r.value))
    }
  }

  def mpqToRat(x : Mpq) : Rational = {
    val num = x.getNum.bigIntegerValue
    val den = x.getDen.bigIntegerValue
    assert(den != 0)
    Rational(num, den)
  }


  def unsafeCoeffToRational(a : Coeff) : Rational = {
    if (a.isZero) {
      Rational(0)
    } else if (a.isScalar) {
      assert(a.inf.isInfty == 0, "Can't convert infty to Rational. Can convert infty to RationalExt, however.")
      val re = scalarToRatExt(a.inf) // Scalar.inf = itself
      assert(!re.isInfinity)
      re.value
    } else {
      ???
    }
  }

  def coeffToRationalExt(a : Coeff) : RationalExt = {
    if (a.isScalar) {
      scalarToRatExt(a.inf)
    } else {
      ???
    }
  }

  def rationalToCoeff(a : Rational) : Coeff = {
    new MpqScalar(ratToMpq(a))
  }

  def lfToLincons(lf : LinearForm) : Lincons0 = {
    val le = SpireGmpUtils.lfToLinexpr(lf * -1)
    new Lincons0(Lincons0.SUPEQ, le) // TODO: Triple check sign!!!
  }

  def linconsToLf(lc : Lincons0) : Seq[LinearForm] = {
    assert(lc.scalar == null) // Don't know how to handle congruences/don't care
    lc.kind match {
      case Lincons0.SUPEQ => Seq(SpireGmpUtils.linexprToLf(lc.expr) * -1)
      case Lincons0.EQ => {
        val lf = SpireGmpUtils.linexprToLf(lc.expr)
        val lf2 = lf * -1
        Seq(lf, lf2)
      }
      case _ => ??? // Don't know/don't care
    }
  }

  def lfToLinexpr(a : LinearForm) : Linexpr0 = {
    val ltrms : Array[Coeff] = a.coeffs.map(ratExtToScalar(_)).toArray
    assert(ltrms.length > 0)
    new Linexpr0(ltrms.tail, ltrms.head)
  }

  def linexprToLf(a : Linexpr0) : LinearForm = {
    /*
     *  This check can be removed if Linexpr0.getCoeffs is patched with
     *
     *  if (l.length == 0) {
     *     Coeff[] c = new Coeff[0];
     *     return c;
     *  } else {
     *     ...
     *
     */
    val coeffs = if (a.getSize() == 0)
      Array[Rational]()
    else
      (a.getCoeffs.map((x) => unsafeCoeffToRational(x)).toArray)
    val cst = unsafeCoeffToRational(a.getCst)
    val ltrms = cst +: coeffs
    assert(ltrms.length > 0)

    LinearForm(ltrms.toList : _*)
  }

}
