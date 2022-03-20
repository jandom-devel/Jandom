/**
 * Copyright 2013, 2016 Gianluca Amato <gamato@unich.it>
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

import org.scalatest.funsuite.AnyFunSuite

import it.unich.jandom.utils.numberext._
import it.unich.jandom.domains.numerical.BoxDoubleDomain
import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.domains.numerical.ParallelotopeRationalDomain

/**
 * The test suite for domain transformations.
 * @author Gianluca Amato <gamato@unich.it>
 */
class DomainTransformationSuite extends AnyFunSuite {
  val boxdom = BoxDoubleDomain()
  val pardom = ParallelotopeRationalDomain()

  test("Parallelotope to BoxDouble") {
    val transform = implicitly[DomainTransformation[ParallelotopeRationalDomain, BoxDoubleDomain]]
    val diamond = pardom(Bounds(-1, -1), DenseMatrix(DenseVector(1.0, 1.0), DenseVector(1.0, -1.0)), Bounds(1, 1))
    val box = boxdom(Array(-1, -1), Array(1, 1))
    assertResult(box) { transform(pardom,boxdom)(diamond) }
  }

  test("Parallelotope to Parallelotope") {
    val transform = implicitly[DomainTransformation[ParallelotopeRationalDomain, ParallelotopeRationalDomain]]
    val diamond = pardom(Bounds(-1, -1), DenseMatrix(DenseVector(1.0, 1.0), DenseVector(1.0, -1.0)), Bounds(1, 1))
    assertResult(diamond) { transform(pardom,pardom)(diamond) }
  }

  test("Box to Parallelotope") {
    val transform = implicitly[DomainTransformation[BoxDoubleDomain, ParallelotopeRationalDomain]]
    val boxptope = pardom(Bounds(-1, -1), DenseMatrix.eye(2), Bounds(1, 1))
    val box = boxdom(Array(-1, -1), Array(1, 1))
    assertResult(boxptope) { transform(boxdom,pardom)(box) }
  }

  test("General transformation to Box") {
    val transform = new DomainTransformation.TopTransformation[NumericalDomain, BoxDoubleDomain]
    val diamond = pardom(Bounds(-1, -1), DenseMatrix(DenseVector(1.0, 1.0), DenseVector(1.0, -1.0)), Bounds(1, 1))
    assertResult(boxdom.top(2)) { transform(pardom,boxdom)(diamond) }
  }
}
