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

package it.unich.jandom.domains.objects

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.prop.Tables._

import it.unich.jandom.objectmodels.ObjectModel
import it.unich.jandom.objectmodels.TestObjectModel

trait AliasingDomainSuiteParameters {
  import scala.language.implicitConversions
  import AliasingDomain._

  val om = TestObjectModel
  val dom = AliasingDomain(om)
  val someTypes = Table[dom.FiberComponent]("type", om.tsuper, om.tsub, om.tother)
  val someFibers = Table[Seq[dom.FiberComponent]]("fiber", Seq(om.tsuper, om.tsuper), Seq(om.tsuper, om.tsuper, om.tsuper), Seq(om.tsuper, om.tsuper, om.tsuper, om.tsuper))

  implicit def sizeToTypes(size: Int) = Seq.fill(size)(om.tsuper)

  val bot4 = dom.bottom(4)
  val top4 = dom.top(4)

  val n0 = Node()
  val n1 = Node()
  val n2 = Node()
  val n3 = Node()
  val n4 = Node()

  val g1 = dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', n2), (n1, 'b', n2)), 4)
  val g1noless = dom(Seq(None, Some(n1), Some(n1), None), Seq((n1, 'b', n2)), 4)
  val g1a = dom(Seq(Some(n2), Some(n1), None, None), Seq((n1, 'b', n0), (n2, 'a', n0)), 4)
  val g1b = dom(Seq(Some(n0), Some(n1), Some(n2), None), Seq((n0, 'a', n2), (n1, 'b', n2)), 4)
  val g1c = dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', n2), (n0, 'c', Node()), (n0, 'd', Node()), (n1, 'b', n2)), om.tsub +: 3)
  val g1d = dom(Seq(Some(n0), Some(n1), Some(n1), None, Some(n3)), Seq((n0, 'a', n2), (n1, 'b', n2), (n3, 'a', Node()), (n3, 'b', Node())), 5)
  val g1e = dom(Seq(Some(n1), Some(n0), None), Seq((n0, 'a', n2), (n1, 'b', n2)), 3)
  val g1f = dom(Seq(Some(n0), Some(n1), None), Seq((n0, 'a', n2), (n1, 'b', n2)), 3)
  val g1bb = dom(Seq(None, Some(Node()), None, None), Seq(), 4)
  val g2 = dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', n2)), 4)
  val g3 = dom(Seq(Some(n2), Some(n1), Some(n1), Some(n3)), Seq((n2, 'a', n0), (n2, 'b', n3)), 4)
  val g4 = dom(Seq(Some(n0), Some(n1), Some(n1), Some(n0)), Seq((n0, 'a', n2), (n0, 'b', n2), (n1, 'b', n0)), 3 :+ om.tsub)
  val g4b = dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', n2)), 3 :+ om.tsub)
  val g4union = dom(Seq(Some(n0), Some(n1), Some(n1), Some(n2)), Seq((n0, 'a', Node()), (n0, 'b', n4), (n1, 'b', n2),
    (n2, 'a', n4), (n2, 'b', n4)), 3 :+ om.tsub)
  val g4big = dom(Seq(Some(n0), Some(n1), Some(n1), Some(n2)), Seq((n0, 'a', Node()), (n0, 'b', Node()), (n1, 'b', Node()),
    (n2, 'a', Node()), (n2, 'b', Node())), 3 :+ om.tsub)
  val g5 = dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', Node()), (n1, 'a', n1), (n1, 'b', Node())), 4)
  val g5big = dom(Seq(Some(n0), Some(n1), Some(n1), None), Seq((n0, 'a', Node()), (n1, 'a', Node()), (n1, 'b', Node())), 4)

  val someProperties = Table("property", g1, g2, g3, g4, g5, /*g1 union g5*,*/ bot4, top4, g1a, g1b, g1c, g1d, g1e, g1f, g1bb)
  val isFirstLevel = Table(("property", "node", "1st-level"), (g1, n0, true), (g1, n1, true), (g1, n2, false), (g1a, n2, true))
  val reachableNodesFrom = Table(("property", "nodes", "reachables"), (g1, Seq(n0), Set(n0, n2)), (g1, Seq(n0,n1), Set(n0,n1,n2)))
}

/**
 * The test suite for the aliasing domain
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class AliasingDomainSuite extends AnyFunSpec with AliasingDomainSuiteParameters with PrivateMethodTester
  with ObjectDomainSuite with PreciseFiberChange with PreciseDefiniteNullness
  with PreciseObjectDomain with PreciseDefiniteWeakAliasing {

  import AliasingDomain._

  def AliasingMorphism(g1: dom.Property, g2: dom.Property, m: Morphism) {
    it("preserve labels") {
      for (i <- 0 until g1.dimension) {
        assert((g1.labels(i) flatMap m) === g2.labels(i))
        for (f <- om.fields(g1.types(i)))
          assert((g1.nodeOf(i, f) flatMap m) === g2.nodeOf(i, f))
      }
    }
  }

  def nonExtremalGraph[OM <: ObjectModel](g: dom.Property) {
    val bottom = g.bottom
    val top = g.top
    describe("has a morphism to bottom which") {
      val Some((1, m)) = g.tryMorphism(bottom)
      it should behave like AliasingMorphism(g, bottom, m)
    }
    describe("has a morphism from top which") {
      val Some((-1, m)) = g.tryMorphism(top)
      it should behave like AliasingMorphism(top, g, m)
    }
    it should behave like nonExtremalProperty(g)
  }

  describe("The isFirstLevel method") {
    it("determines whether a node is 1st level") {
      forAll(isFirstLevel) { (prop, node, firstlevel) =>
        assertResult(firstlevel)(prop.isFirstLevel(node))
      }
    }
    it("returns correct results") {
      forAll(someProperties) { (prop) =>
        for (n <- prop.nodes; if prop.isFirstLevel(n)) assert(prop.labels.contains(Some(n)))
      }
    }
  }

  describe("The reachableNodesFrom") {
    it("determines the of nodes reachable from a given first level node") {
      forAll(reachableNodesFrom) { (prop, nodes, reachable) =>
        assertResult(reachable)(prop.reachableNodesFrom(nodes :_*))
      }
    }
    it("always contains the starting node") {
      forAll(someProperties) { (prop) =>
        for (n <- prop.nodes; if prop.isFirstLevel(n)) assert(prop.reachableNodesFrom(n) contains n)
      }
    }
  }

  describe("The bottom aliasing graph") {

    it("has all reachable identifiers labeled by null") {
      forAll(someFibersAndVars) { (fiber, i) =>
        val bot = dom.bottom(fiber)
        assert(bot.labels(i).isEmpty)
        for (f <- om.fields(om.tsuper)) {
          assert(bot.nodeOf(i, f).isEmpty)
        }
      }
    }
    it("has all identifiers definitively null") {
      forAll(someFibersAndVars) { (fiber, i) =>
        val bot = dom.bottom(fiber)
        assert(bot.mustBeNull(i))
        for (j <- om.fields(om.tsuper)) {
          assert(bot.mustBeNull(i), Seq(j))
          for (k <- om.fields(om.typeOf(j))) assert(bot.mustBeNull(i), Seq(j, k))
        }
      }
    }
    it("is strictly smaller than top") {
      forAll(someFibers) { (fiber) =>
        dom.bottom(fiber) < dom.top(fiber)
      }
    }
    it("is not top") {
      forAll(someFibers) { (fiber) =>
        assert(!dom.bottom(fiber).isTop)
      }
    }

    it("is not empty") {
      forAll(someFibers) { (fiber) =>
        assert(!dom.bottom(fiber).isEmpty)
      }
    }
  }

  describe("The top aliasing graph") {
    it("has all reachable identifiers mapped to a node") {
      forAll(someFibersAndVars) { (fiber, i) =>
        val top = dom.top(fiber)
        assert(top.labels(i).nonEmpty)
        for (f <- om.fields(om.tsuper)) {
          assert(top.nodeOf(i, f).nonEmpty)
        }
      }
    }
    it("is strictly bigger than bottom") {
      forAll(someFibers) { (fiber) =>
        dom.top(fiber) > dom.bottom(fiber)
      }
    }
    it("is not bottom") {
      forAll(someFibers) { (fiber) =>
        assert(!dom.top(fiber).isBottom)
      }
    }
  }

  describe("The graph g1") {
    it should behave like nonExtremalGraph(g1)
    it("has dimension 4") { assert(g1.dimension === 4) }
    it("is not comparable with g1noless") { assert(g1.tryCompareTo(g1noless).isEmpty) }
    it("is not comparable with g2") { assert(g1.tryCompareTo(g2).isEmpty) }
    it("is not comparable with g3") { assert(g1.tryCompareTo(g3).isEmpty) }
  }

  describe("The graph g2") {
    it should behave like nonExtremalGraph(g2)
    it("has dimension 4") { assert(g2.dimension === 4) }
    it("is not comparable with g1") { assert(g2.tryCompareTo(g1).isEmpty) }
    it("is smaller than g3") { assert(g2 < g3) }
    describe("has a morphism from g3 which") {
      val Some((-1, m)) = g2.tryMorphism(g3)
      it should behave like AliasingMorphism(g3, g2, m)
    }
  }

  describe("The graph g4") {
    it should behave like nonExtremalGraph(g4)
    it("has dimension 4") { assert(g4.dimension === 4) }
    it("is smaller than g4big") { assert(g4 < g4big) }
  }

  describe("The graph g5") {
    it should behave like nonExtremalGraph(g5)
    it("has dimension 4") { assert(g5.dimension === 4) }
    it("is smaller than g5big") { assert(g5 < g5big) }
  }

  describe("The nodeType method") {
    it("returns type t for nodes bounds to variables all of type t") {
      assert((g4.nodeType(g4.labels(1).get)) === om.tsuper)
    }
    it("returns the least type of all variables bound to the node") {
      assert((g4.nodeType(g4.labels(0).get)) === om.tsub)
    }
    /*it("returns None for nodes not bound to variables") {
      assert((g4.nodeType(g4.nodeOf(0, 'a').get)) === None)
    }*/
  }

  describe("The expandSpan method") {
    val span = g1.labels(0).get
    val newspan = g1.expandSpan(span, om.tsub)
    it("adds c field when moving from tsuper to tsub") {
      assert(newspan isDefinedAt 'c')
    }
    it("does not add null fields") {
      assert(!(newspan isDefinedAt 'b'))
    }
  }

  describe("The assignNull method") {
    it("maps g1.assignNull(2) to g1a") {
      assert(g1.assignNull(2) === g1a)
    }
  }

  describe("The assignVariable method") {
    it("maps g1.assignVariable(1,2) to g1") {
      assert(g1.assignVariable(1, 2) === g1)
    }
    it("maps g1.assignVariable(2,3) to g1a") {
      assert(g1.assignVariable(2, 3) === g1a)
    }
  }

  describe("The assignVariableToField method") {
    it("it maps g1.assignVariableToField(1, 'b', 3) to g2") {
      assert(g1.assignVariableToField(1, 'b', 3) === g2)
    }
    it("it maps g1.assignVariableToField(2, 'a', 2) to g5") {
      assert(g1.assignVariableToField(2, 'a', 2) === g5)
    }
  }

  describe("The cast method") {
    it("maps g1.castVariable(0,tsub) to g1c") {
      assert(g1.castVariable(0, om.tsub) === g1c)
    }
  }

  describe("The addFreshVariable method") {
    it("maps g1.addFreshVariable(tsuper) to g1d") {
      assert(g1.addFreshVariable(om.tsuper) === g1d)
    }
  }

  describe("The mapVariables method") {
    it("maps g1.mapVariables(Seq(1, -1, 0, 2)) to g1e") {
      assert(g1.mapVariables(Seq(1, -1, 0, 2)) === g1e)
    }
    it("maps g1.mapVariables(Seq(0, 1,  -1, 2)) to g1f") {
      assert(g1.mapVariables(Seq(0, 1, -1, 2)) === g1f)
    }
  }

  describe("The testNull method") {
    it("maps g1b.testNull(0) to g1bb") {
      assert(g1b.testNull(0) === g1bb)
    }
  }

  describe("The union method") {
    it("yields g4union when applied to g4 union g4b") {
      assert((g4b union g4) === g4union)
      assert((g4 union g4b) === g4union)
    }
  }

  describe("The connect method") {
    it("passes test1") {
      val n0 = Node()
      val n1 = Node()
      val n2 = Node()
      val g1 = dom(Seq(Some(n0), Some(n0), Some(n1)), Seq(), 3)
      val g2 = dom(Seq(Some(n2)), Seq(), 1)
      val g3 = dom(Seq(Some(n0), Some(n0)), Seq ((n0, 'a', Node()),(n0,'b',Node())), 2)
      assert(g1.connect(g2, 1) === g3)
    }
    it("passes test2") {
      val n0 = Node()
      val n1 = Node()
      val n2 = Node()
      val n3 = Node()
      val g1 = dom(Seq(Some(n0), Some(n0), Some(n1)), Seq((n0, 'a', Node())), 3)
      val g2 = dom(Seq(Some(n2), Some(n3)), Seq((n3, 'b', n2)), 2)
      val g3 = dom(Seq(Some(n0), Some(n0), Some(n3)), Seq((n0, 'a', Node()), (n0, 'b', Node()), (n3, 'b', n2)), 3)
      assert(g1.connect(g2, 1) === g3)
    }
  }
}
