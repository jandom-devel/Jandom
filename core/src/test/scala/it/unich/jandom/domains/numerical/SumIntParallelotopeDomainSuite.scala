/**
 * Copyright 2014 Gianluca Amato
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.domains.numerical

import org.scalatest.FunSpec

import it.unich.jandom.domains.{EmptyExistsSuite, SeparatedTopAndBottomSuite}

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

/**
 * Test suite for the sum of interval and parallelotope domains.
 * @author Gianluca Amato <gamato@unich.it>
 */
class SumIntParallelotopeDomainSuite extends FunSpec  with NumericalDomainSuite with SeparatedTopAndBottomSuite with EmptyExistsSuite {
  lazy val dom = SumIntParallelotopeDomain()

  val box1 = dom.dom1(Array(-1, -1),Array(1,1))

  val par1 = dom.dom2(DenseVector(-1,-1), DenseMatrix.eye(2), DenseVector(1,1))
  val par2 = dom.dom2(DenseVector(-1, -1), DenseMatrix((1.0, 1.0), (1.0, -1.0)), DenseVector(1, 1))

  override lazy val someProperties = Table("property", dom.bottom(0), dom.bottom(1), dom.bottom(2), dom.bottom(3), dom.bottom(4), dom.bottom(4),
    dom.top(0), dom.top(1), dom.top(2), dom.top(3), dom.top(4), dom.top(5), dom(box1, par1), dom(box1, par2))

  describe("All objects in Int + Parallelotope are polyhedral") {
    forAll(someProperties) { (p) => assert(p.isPolyhedral) }
  }
}
