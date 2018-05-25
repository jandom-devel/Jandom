package it.unich.jandom.domains.numerical
import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.utils.dbm._
import it.unich.jandom.domains.numerical.octagon._
import it.unich.jandom.utils.numberext.RationalExt
import spire.math.Rational
import scala.reflect.ClassTag

class OctagonDomain(val boxDomain : BoxGenericDomain[RationalExt]) extends NumericalDomain {
  type Property = OctagonProperty
  implicit val ifield = RationalExt
  implicit val tag = implicitly[ClassTag[RationalExt]]
  implicit val factory = new ArrayDBMFactory()(tag, ifield)
  implicit val closure = new IncrementalMineFloydWarshall[RationalExt]()(ifield, factory)
  implicit val octDomain : OctagonDomain = this
  implicit val box = BoxRationalDomain()
  def top(dim : Int) : Property = ???
  def bottom(dim : Int): Property = ???

  val widenings = Seq(WideningDescription.default[Property])

  def fromBox (b : BoxRationalDomain#Property) : OctagonProperty =
    if (b.isEmpty)
      bottom(b.dimension)
    else {
      val lower = b.low.zip(1 to b.dimension).foldRight(this.top(b.dimension))(
        (i, z : OctagonProperty) =>
          if (i._1.isNegInfinity) z
          else z.linearInequality(LinearForm(Array.fill[Rational](b.dimension + 1)(0).updated(i._2, Rational(-1)).updated(0, i._1.value) : _*)))

      b.high.zip(1 to b.dimension).foldRight(lower)((i, z : OctagonProperty) =>
          if (i._1.isPosInfinity) z
          else z.linearInequality(LinearForm(Array.fill[Rational](b.dimension + 1)(0).updated(i._2, Rational(1)).updated(0, -i._1.value) : _*)))
    }
}

object OctagonDomain {
  val boxDomain = BoxRationalDomain()
  def apply() = new OctagonDomain(boxDomain)
}
