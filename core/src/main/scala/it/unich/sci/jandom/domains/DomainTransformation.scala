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
 * This is the trait for domain transformations, i.e. maps from one domain to
 * another. This is just a marker trait.
 * @tparam A source domain property
 * @tparam B target domain property
 * @author Gianluca Amato <gamato@unich.it>
 */
trait DomainTransformation[A, B] extends Function[A, B]

/**
 * This object is a collection of standard domain transformations.
 * @todo Evaluate whether collecting all domain transformations in this object is a good
 * architectural choice.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Francesca Scozzari <fscozzari@unich.it>
 */
object DomainTransformation {
  implicit object ParallelotopeToBoxDouble extends DomainTransformation[Parallelotope, BoxDouble] {
    import breeze.linalg.{ DenseMatrix, DenseVector }
    def apply(x: Parallelotope) = {
      val newPar = x.rotate(DenseMatrix.eye(x.dimension))
      if (newPar.isEmpty)
        BoxDouble.bottom(newPar.dimension)
      else
        BoxDouble(newPar.low.toArray, newPar.high.toArray)
    }
  }

  implicit object BoxDoubleToParallelotope extends DomainTransformation[BoxDouble, Parallelotope] {
    import breeze.linalg.{ DenseMatrix, DenseVector }
    def apply(x: BoxDouble) = {
      Parallelotope(DenseVector(x.low), DenseMatrix.eye(x.dimension), DenseVector(x.high))
    }
  }

  implicit object ParallelotopeToParallelotope extends DomainTransformation[Parallelotope, Parallelotope] {
    def apply(x: Parallelotope) = x
  }

  implicit object BoxDoubleToBoxDouble extends DomainTransformation[BoxDouble, BoxDouble] {
    def apply(x: BoxDouble) = x
  }

  implicit object NumericalPropertyToBoxDouble extends DomainTransformation[NumericalProperty[_], BoxDouble] {
    def apply(x: NumericalProperty[_]) = {
      BoxDouble.top(x.dimension)
    }
  }
}
