/**
 * Copyright 2013, 2016 Gianluca Amato, Francesca Scozzari
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

package it.unich.jandom.domains

import it.unich.jandom.domains.numerical._
import it.unich.jandom.utils.breeze.RationalForBreeze._
import it.unich.jandom.utils.numberext.RationalExt

/**
 * This is the trait for domain transformations, i.e. maps from properties of one abstract domain to
 * properties of another abstract domain. Domain transformations are parametrized w.r.t. a family of
 * abstract domains.
 * @tparam DomA source family of abstract domains
 * @tparam DomB target family of abstract domains
 * @author Gianluca Amato <gamato@unich.it>
 */
trait DomainTransformation[-DomA <: AbstractDomain, -DomB <: AbstractDomain] extends {
  /**
   * This function returns the real map from source to target property.
   * @param src the source domain
   * @param dst the target domain
   * @return the map from source to target properties
   */
  def apply(src: DomA, dst: DomB): src.Property => dst.Property
}

/**
 * This object is a collection of standard domain transformations.
 * @todo Evaluate whether collecting all domain transformations in this object is a good
 * architectural choice.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
object DomainTransformation {
  implicit object ParallelotopeToBoxDouble extends DomainTransformation[ParallelotopeDomain, BoxDoubleDomain] {
    import breeze.linalg.DenseMatrix
    def apply(src: ParallelotopeDomain, dst: BoxDoubleDomain): src.Property => dst.Property = { (x) =>
      val newPar = x.rotate(DenseMatrix.eye(x.dimension))
      if (newPar.isEmpty)
        dst.bottom(newPar.dimension)
      else
        dst(newPar.low.toArray, newPar.high.toArray)
    }
  }

  implicit object ParallelotopeRationalToBoxDouble extends DomainTransformation[ParallelotopeRationalDomain, BoxDoubleDomain] {
    import breeze.linalg.DenseMatrix
    def apply(src: ParallelotopeRationalDomain, dst: BoxDoubleDomain): src.Property => dst.Property = { (x) =>
      val newPar = x.rotate(DenseMatrix.eye(x.dimension))
      if (newPar.isEmpty)
        dst.bottom(newPar.dimension)
      else
        dst(newPar.low.toArray map (_.toDouble), newPar.high.toArray map (_.toDouble))
    }
  }

  implicit object ParallelotopeRationalToBoxRational extends DomainTransformation[ParallelotopeRationalDomain, BoxRationalDomain] {
    import breeze.linalg.DenseMatrix
    def apply(src: ParallelotopeRationalDomain, dst: BoxRationalDomain): src.Property => dst.Property = { (x) =>
      val newPar = x.rotate(DenseMatrix.eye(x.dimension))
      if (newPar.isEmpty)
        dst.bottom(newPar.dimension)
      else
        dst(newPar.low.toArray, newPar.high.toArray)
    }
  }

  implicit object BoxDoubleToParallelotope extends DomainTransformation[BoxDoubleDomain, ParallelotopeDomain] {
    import breeze.linalg.{ DenseMatrix, DenseVector }
    def apply(src: BoxDoubleDomain, dst: ParallelotopeDomain): src.Property => dst.Property = { (x) =>
      dst(DenseVector(x.low), DenseMatrix.eye(x.dimension), DenseVector(x.high))
    }
  }

  implicit object BoxDoubleToParallelotopeRational extends DomainTransformation[BoxDoubleDomain, ParallelotopeRationalDomain] {
    import breeze.linalg.{ DenseMatrix, DenseVector }
    def apply(src: BoxDoubleDomain, dst: ParallelotopeRationalDomain): src.Property => dst.Property = { (x) =>
      dst(DenseVector(x.low map { RationalExt(_) }), DenseMatrix.eye(x.dimension), DenseVector(x.high map { RationalExt(_) }))
    }
  }

  implicit object BoxRationalToParallelotopeRational extends DomainTransformation[BoxRationalDomain, ParallelotopeRationalDomain] {
    import breeze.linalg.{ DenseMatrix, DenseVector }
    def apply(src: BoxRationalDomain, dst: ParallelotopeRationalDomain): src.Property => dst.Property = { (x) =>
      dst(DenseVector(x.low), DenseMatrix.eye(x.dimension), DenseVector(x.high))
    }
  }

  implicit object ParallelotopeToParallelotope extends DomainTransformation[ParallelotopeDomain, ParallelotopeDomain] {
    def apply(src: ParallelotopeDomain, dst: ParallelotopeDomain): src.Property => dst.Property = { (x) => new dst.Property(x.isEmpty, x.low, x.A, x.high) }
  }

  implicit object ParallelotopeRationalParallelotopeRational extends DomainTransformation[ParallelotopeRationalDomain, ParallelotopeRationalDomain] {
    def apply(src: ParallelotopeRationalDomain, dst: ParallelotopeRationalDomain): src.Property => dst.Property = { (x) => new dst.Property(x.isEmpty, x.low, x.A, x.high) }
  }

  implicit object BoxDoubleToBoxDouble extends DomainTransformation[BoxDoubleDomain, BoxDoubleDomain] {
    def apply(src: BoxDoubleDomain, dst: BoxDoubleDomain): src.Property => dst.Property = { (x) => dst(x.low, x.high) }
  }

  implicit object BoxRationalToBoxRational extends DomainTransformation[BoxRationalDomain, BoxRationalDomain] {
    def apply(src: BoxRationalDomain, dst: BoxRationalDomain): src.Property => dst.Property = { (x) => { dst(x.low, x.high) } }
  }

  /**
   * This is a class for domain transformations which transform every object of the souece domin into the top object of the
   * target domain.
   */
  class TopTransformation[Src  <: NumericalDomain, Dst <: NumericalDomain] extends DomainTransformation[Src, Dst] {
    def apply(src: Src, dst: Dst): src.Property => dst.Property = { (x) => dst.top(x.dimension) }
  }
}
