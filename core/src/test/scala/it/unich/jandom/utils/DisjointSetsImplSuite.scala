/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.utils

import org.scalatest.funspec.AnyFunSpec

class DisjointSetsImplSuite extends AnyFunSpec {
  describe("A DisjointSetImpl") {
    it("should put initial elements in disjoint sets") {
      val ds = DisjointSetsImpl(1 until 10: _*)
      assert((1 until 10) forall { i => ds.find(i) == Some(i) })
      assert(ds.size == 9)
      assert(ds.setCount === 9)
    }
    it("should remove all elements after clear") {
      val ds = DisjointSetsImpl(1 until 10: _*)
      ds.clear()
      assert((1 until 10) forall { i => ds.find(i) == None })
      assert(ds.size === 0)
      assert(ds.setCount === 0)
    }
    it("should add new elements in disjoint sets") {
      val ds = DisjointSetsImpl[Int]()
      (1 until 10) foreach { ds += _ }
      assert((1 until 10) forall { i => ds.find(i) == Some(i) })
    }
    it("should union two sets when required") {
      val ds = DisjointSetsImpl(1 until 10: _*)
      (1 until 5) foreach { i => ds.union(i, i + 1) }
      (6 until 9) foreach { i => ds.union(i, i + 1) }
      assert((1 to 5) forall { i => ds.find(i) == ds.find(1) })
      assert((6 to 9) forall { i => ds.find(i) == ds.find(9) })
      assert(ds.find(1) != ds.find(9))
      assert(ds.setCount === 2)
    }
    it("should automaically add new elemets during union") {
      val ds = DisjointSetsImpl[Int]()
      ds.union(1, 2)
      assert (ds.size === 2)      
      ds.union(2, 3)
      assert (ds.size === 3)
      ds.union(3, 4)
      assert (ds.size === 4)
      assert((1 to 4) forall { i => ds.find(i) == ds.find(1) })
    }
    it("should add a single copy of a new element during join") {
      val ds = DisjointSetsImpl[Int]()
      ds.union(1,1)
      assert(ds.size == 1)
    } 
    it("should join elements using rank") {
      val ds = DisjointSetsImpl[Int]()
      ds.union(1, 2)
      ds.union(3, 4)
      ds.union(5, 6)
      ds.union(3, 5)
      ds.union(1, 3)
      assert((1 to 6) forall { i => ds.find(i) == Some(3) })
    }
    it("should correctly identify elements in the same partition") {
      val ds = DisjointSetsImpl[Int]()
      ds.union(1, 2)
      ds.union(3, 4)
      assert(ds.inSamePartition(1, 2))
      assert(ds.inSamePartition(3, 4))
      assert(! ds.inSamePartition(1, 4))
    }
    it("should consider new elements are in different partitions") {
      val ds = DisjointSetsImpl[Int]()
      ds.union(1, 2)
      ds.union(3, 4)
      assert(! ds.inSamePartition(1, 6))
      assert(! ds.inSamePartition(8, 6))
      assert(! ds.inSamePartition(6, 1))
    }
  }
}
