package it.unich.jandom.domains.numerical
import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.utils.dbm._
import it.unich.jandom.domains.numerical.octagon._
import it.unich.jandom.domains.numerical.octagon.optimized._
import it.unich.jandom.utils.numberext.RationalExt
import spire.math.Rational
import scala.reflect.ClassTag

trait OctagonDomainTrait[O <: Octagon[RationalExt, O]] extends NumericalDomain {
  def makeTop(dim : Int): O
  def makeBottom(dim: Int): O
}

abstract class OctagonDomain[O <: Octagon[RationalExt, O]] extends OctagonDomainTrait[O] {
  implicit val ifield = RationalExt

  val mod = new OctagonPropertyModule[O, OctagonDomain[O]](this)

  type Property = mod.OctagonProperty
  implicit val tag = implicitly[ClassTag[RationalExt]]
  implicit val factory = new ArrayDBMFactory()(tag, ifield)
  implicit val closure = new IncrementalMineFloydWarshall[RationalExt]()(ifield, factory)
  val box = BoxRationalDomain()
  def top(dim : Int) : Property = mod.OctagonProperty(this.makeTop(dim))
  def bottom(dim : Int): Property = mod.OctagonProperty(this.makeBottom(dim))
  val widenings = Seq(WideningDescription.default[Property])

  def fromBox(b : BoxRationalDomain#Property) : mod.OctagonProperty = mod.fromBox(b)

}

object OctagonDomain {
  def apply() = new OctagonDomain[OptimizedOctagon[RationalExt]] {
    def makeTop(dim : Int) = new CachingOctagon(Right(factory.top((OctagonDim(dim).toDBMDim))))
    def makeBottom(dim: Int) = new BottomOptOcta(OctagonDim(dim))
  }
}
