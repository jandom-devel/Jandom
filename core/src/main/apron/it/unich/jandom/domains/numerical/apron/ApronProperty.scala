package it.unich.jandom.domains.numerical.apron

import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.utils.numberext.RationalExt
import it.unich.jandom.domains.numerical.Inequalities
import apron._

trait ApronNumericalDomainAdapter [A <: Manager] {
  def top(n : Int) : Abstract0
  def bottom(n : Int) : Abstract0
  def dimension(obj : Abstract0) : Int
}

class ApronIntAdapter[A <: Manager](man : A) extends ApronNumericalDomainAdapter[A] {
  def top(n : Int) = new Abstract0(man, n, 0)
  def bottom(n : Int) = new Abstract0(man, n, 0, true)
  def dimension(obj : Abstract0) : Int = obj.getDimension(man).intDim
}

class ApronRealAdapter[A <: Manager](man : A) extends ApronNumericalDomainAdapter[A] {
  def top(n : Int) = new Abstract0(man, 0, n)
  def bottom(n : Int) = new Abstract0(man, 0, n, true)
  def dimension(obj : Abstract0) : Int = obj.getDimension(man).realDim
}


class ApronProperty[A <: Manager, F <: ApronNumericalDomainAdapter[A]] (val domain: ApronDomain[A, F], val obj : Abstract0)(implicit val apronnum : F)
    extends NumericalProperty[ApronProperty[A, F]] {
  private val kDebugConstr : Boolean = true  // TODO: Move elsewhere or add option to GUI
  type Domain = ApronDomain[A, F]
  type Property = ApronProperty[A, F]

  def widening(that: Property): Property = if (true) this.top else {
    val res = new Property(this.domain, this.obj.widening(this.domain.manager, that.obj))
    assert(res.obj.getDimension(this.domain.manager) == this.obj.getDimension(this.domain.manager))
    res
  }

  def narrowing(that: Property): Property = this
  // after PPLProperty.narrowing; Japron doesn't expose narrowing
  // TODO: Come up with something better or patch Japron

  def union(that: Property): Property = {
    assert(that.obj.getDimension(this.domain.manager) == this.obj.getDimension(this.domain.manager))
    val res = new Property(this.domain, that.obj.joinCopy(this.domain.manager, this.obj))
    assert(res.obj.getDimension(this.domain.manager) == this.obj.getDimension(this.domain.manager))
    res
  }

  def intersection(that: Property): Property = {
    val res = new Property(this.domain, that.obj.meetCopy(this.domain.manager, this.obj))
    assert(res.obj.getDimension(this.domain.manager) == this.obj.getDimension(this.domain.manager))
    res
  }

  def nonDeterministicAssignment(n: Int): Property = {
    require(n < dimension && n >= 0)
    if (isEmpty)
      this
    else
      new Property(this.domain, this.obj.forgetCopy(this.domain.manager, Array(n), false))
  }

  // Abstract assignment (Mine sects. 4.2, 4.4)
  def linearAssignment(n: Int, lf: LinearForm): Property = {
    assert(n <= dimension)
    new Property(this.domain, obj.assignCopy(this.domain.manager, n, SpireGmpUtils.lfToLinexpr(lf), null))
  }

  // Abstract test (Mine sect. 4.5)
  def linearInequality(lf: LinearForm): Property = {
    new ApronProperty(this.domain, obj.meetCopy(this.domain.manager, SpireGmpUtils.lfToLincons(lf)))
  }

  def linearDisequality(lf: LinearForm): Property = ??? // TODO

  def toInterval : domain.boxDomain.Property =
  {
    val box = this.obj.toBox(this.domain.manager)
    val inf  = box.map(x => SpireGmpUtils.scalarToRatExt(x.inf()))
    val sup  = box.map(x => SpireGmpUtils.scalarToRatExt(x.sup()))
    val isNotEmpty = (inf zip sup).foldLeft(true)((z,i) => z & (!(i._1.isPosInfinity) && !(i._2.isNegInfinity)))
    if (isNotEmpty)
      domain.boxDomain.makeBox(inf, sup, false)
    else {
      val pairs = Array.fill(inf.size)((RationalExt.PositiveInfinity, RationalExt.NegativeInfinity))
      domain.boxDomain.makeBox(pairs.unzip._1, pairs.unzip._2, true)
    }
  }

  def minimize(lf: LinearForm) = ??? // TODO

  def maximize(lf: LinearForm) = ??? // TODO

  def frequency(lf: LinearForm) = ??? // TODO

  def constraints : Seq[LinearForm] = {
    this.obj.toLincons(this.domain.manager).toSeq.flatMap(SpireGmpUtils.linconsToLf(_)).map(_.padded(dimension+1))
  }

  def isPolyhedral = ??? // TODO

  def addVariable = ??? // TODO

  def delVariable(n: Int) = ??? // TODO

  def mapVariables(rho: Seq[Int]) = ??? // TODO

  def dimension: Int = apronnum.dimension(this.obj)

  def isEmpty = isBottom

  def isTop = this.obj.isTop(this.domain.manager)

  def isBottom = this.obj.isBottom(this.domain.manager)

  def bottom = domain.bottom(this.dimension)

  def top = domain.top(this.dimension)

  // Copypasted from PPL
  def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
    other match {
      case other: ApronProperty[_, _] => {
        assert (obj.getClass == other.obj.getClass)
        if (obj equals other.obj)
          Option(0)
        else if (other.obj.isIncluded(this.domain.manager, this.obj) && (this.obj.isIncluded(this.domain.manager, other.obj)))
          Option(0)
        else if (other.obj.isIncluded(this.domain.manager, this.obj))
          Option(1)
        else if (this.obj.isIncluded(this.domain.manager, other.obj))
          Option(-1)
        else {
          assert(false)
          Option.empty
        }
      }
      case _ => {
        assert(false)
        Option.empty
      }
    }

  override def hashCode: Int = obj.hashCode

  def mkString(vars: Seq[String]): String = {
    if (isTop) {
      "[ T ]"
    } else if (isBottom) {
      "[ _|_ ]"
    } else {
      "[ " +
      Inequalities.constraintsToInequalities(constraints).map(_.mkString(vars)).mkString("; ")
    } +
      (if (!kDebugConstr) {
      " ]"
    } else {
      " ]        //" +
      constraints
    })
  }
}
