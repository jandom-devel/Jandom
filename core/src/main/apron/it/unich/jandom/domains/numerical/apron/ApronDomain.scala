package it.unich.jandom.domains.numerical.apron
import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.domains.numerical.BoxGenericDomain
import it.unich.jandom.domains.numerical.BoxRationalDomain
import it.unich.jandom.utils.numberext.RationalExt
import apron._

class ApronDomain[A <: Manager, N <: ApronNumericalDomainAdapter[A]](val manager : A, val boxDomain : BoxGenericDomain[RationalExt], val apronnum : N) extends NumericalDomain {

  type Property = ApronProperty[A, N]

  def top(n: Int): Property = new ApronProperty(this, apronnum.top(n))(apronnum)
  def bottom(n: Int): Property = new ApronProperty(this, apronnum.bottom(n))(apronnum)

  val widenings = Seq(WideningDescription.default[Property])
}

object ApronIntOctagonDomain {
  val octman : Octagon = new Octagon()
  val boxDomain = BoxRationalDomain()
  val apronint : ApronIntAdapter[Octagon] = new ApronIntAdapter(octman)
  def apply() = new ApronDomain[Octagon, ApronIntAdapter[Octagon]](octman, boxDomain, apronint)
}


object ApronRealOctagonDomain {
  val octman : Octagon = new Octagon()
  val boxDomain = BoxRationalDomain()
  val apronreal : ApronRealAdapter[Octagon] = new ApronRealAdapter(octman)
  def apply() = new ApronDomain[Octagon, ApronRealAdapter[Octagon]](octman, boxDomain, apronreal)
}
