/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

import org.scalatest.FunSuite
import it.unich.sci.jandom.domains.numerical.Parallelotope
import it.unich.sci.jandom.domains.numerical.BoxDoubleDomain
import it.unich.sci.jandom.domains.numerical.NumericalDomain
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix


/**
 * The test suite for domain transformations.
 * @author Gianluca Amato <gamato@unich.it>
 */
class DomainTransformationSuite extends FunSuite {
  val boxdom = new BoxDoubleDomain(false)

  test("Parallelotope to BoxDouble") {
    val transform = implicitly[DomainTransformation[Parallelotope.type, BoxDoubleDomain]]
    val diamond = Parallelotope(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (1.0, -1.0)), DenseVector(1, 1))
    val box = boxdom(Array(-1, -1), Array(1, 1))
    expectResult(box) { transform(diamond,boxdom) }
  }

  test("Parallelotope to Parallelotope") {
    val transform = implicitly[DomainTransformation[Parallelotope.type, Parallelotope.type]]
    val diamond = Parallelotope(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (1.0, -1.0)), DenseVector(1, 1))
    expectResult(diamond) { transform(diamond,Parallelotope) }
  }

  test("Box to Parallelotope") {
    val transform = implicitly[DomainTransformation[BoxDoubleDomain, Parallelotope.type]]
    val boxptope = Parallelotope(DenseVector(-1, -1), DenseMatrix.eye(2), DenseVector(1, 1))
    val box = boxdom(Array(-1, -1), Array(1, 1))
    expectResult(boxptope) { transform(box,Parallelotope) }
  }

  test("General transformation to Box") {
    val transform = implicitly[DomainTransformation[NumericalDomain, BoxDoubleDomain]]
    val diamond = Parallelotope(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (1.0, -1.0)), DenseVector(1, 1))
    val box = boxdom(Array(-1, -1), Array(1, 1))
    expectResult(boxdom.top(2)) { transform(diamond.asInstanceOf[NumericalDomain#Property],boxdom) }
  }
}
