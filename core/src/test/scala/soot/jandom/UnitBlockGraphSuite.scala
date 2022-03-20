/**
  * Copyright 2013, 2018 Gianluca Amato <gianluca.amato@unich.it>
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

package soot.jandom

import scala.collection.JavaConverters._

import org.scalatest.funspec.AnyFunSpec
import soot.toolkits.graph.BriefUnitGraph
import it.unich.jandom.targets.SootTests

/**
  * Test suite for [[soot.jandom.UnitBlockGraph]].
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */

class UnitBlockGraphSuite extends AnyFunSpec with SootTests {
  private val scene = initSoot()
  private val c = scene.loadClassAndSupport("javatest.SimpleTest")

  for (m <- c.getMethods.asScala; body = m.retrieveActiveBody()) {

    describe("The UnitBlockGraph for the " + m.getName + " method") {
      val graph = new UnitBlockGraph(body)
      val unitGraph = new BriefUnitGraph(body)

      it("should only have blocks with head equal to tail") {
        for (b <- graph.asScala) assert(b.getHead === b.getTail)
      }

      it("should be isomorphic to the unit graph") {
        assert(graph.size() === unitGraph.size())
        for (b <- graph.asScala; u = b.getHead) {
          assert((graph.getPredsOf(b).asScala map (_.getHead)) === unitGraph.getPredsOf(u).asScala)
          assert((graph.getSuccsOf(b).asScala map (_.getHead)) === unitGraph.getSuccsOf(u).asScala)
        }
      }
    }
  }
}
