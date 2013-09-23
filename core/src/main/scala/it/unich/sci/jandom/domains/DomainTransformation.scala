/**
 * Copyright 2013 Gianluca Amato, Francesca Scozzari
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

package it.unich.sci.jandom.domains

import it.unich.sci.jandom.domains.numerical._

/**
 * This is the trait for domain transformations, i.e. maps from one domain to another.
 * @tparam DomA source domain
 * @tparam DomB target domain
 * @author Gianluca Amato <gamato@unich.it>
 */
trait DomainTransformation[DomA <: AbstractDomain, DomB <: AbstractDomain] {
  /**
   * This applies the real transformation.
   * @param d the target domain
   * @param x the source property
   * @return the translation of x in the domain d
   */
  def apply(x: DomA#Property, d: DomB): d.Property
}

/**
 * This object is a collection of standard domain transformations.
 * @todo Evaluate whether collecting all domain transformations in this object is a good
 * architectural choice.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
object DomainTransformation {
  implicit object ParallelotopeToBoxDouble extends DomainTransformation[Parallelotope.type, BoxDoubleDomain] {
    import breeze.linalg.{ DenseMatrix, DenseVector }
    def apply(x: Parallelotope, boxdom: BoxDoubleDomain): boxdom.Property = {
      val newPar = x.rotate(DenseMatrix.eye(x.dimension))
      if (newPar.isEmpty)
        boxdom.bottom(newPar.dimension)
      else
        boxdom(newPar.low.toArray, newPar.high.toArray)
    }
  }

  implicit object BoxDoubleToParallelotope extends DomainTransformation[BoxDoubleDomain, Parallelotope.type] {
    import breeze.linalg.{ DenseMatrix, DenseVector }
    def apply(x: BoxDoubleDomain#Property, ptopedom: Parallelotope.type) = {
      Parallelotope(DenseVector(x.low), DenseMatrix.eye(x.dimension), DenseVector(x.high))
    }
  }

  implicit object ParallelotopeToParallelotope extends DomainTransformation[Parallelotope.type, Parallelotope.type] {
    def apply(x: Parallelotope, ptopedom: Parallelotope.type) = x
  }

  implicit object BoxDoubleToBoxDouble extends DomainTransformation[BoxDoubleDomain, BoxDoubleDomain] {
    def apply(x: BoxDoubleDomain#Property, boxdom: BoxDoubleDomain): boxdom.Property = boxdom(x.low,x.high)
  }

  implicit object NumericalPropertyToBoxDouble extends DomainTransformation[NumericalDomain, BoxDoubleDomain] {
    def apply(x: NumericalDomain#Property, boxdom: BoxDoubleDomain): boxdom.Property = boxdom.top(x.dimension)
  }
}
