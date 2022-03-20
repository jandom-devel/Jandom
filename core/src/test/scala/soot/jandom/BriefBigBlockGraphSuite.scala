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
import it.unich.jandom.targets.SootTests

/**
 * Test suite for the [[soot.Jandom.BriefBigBlockGraph]].
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class BriefBigBlockGraphSuite extends AnyFunSpec with SootTests {
  val scene = initSoot()
  val c = scene.loadClassAndSupport("javatest.SimpleTest")

  describe("The BriefBigBlockGraph for the nested method") {
    val body = c.getMethodByName("nested").retrieveActiveBody()
    val graph = new BriefBigBlockGraph(body).asScala
    val expectedGraph = Seq(
      (Seq(), Seq(1)),
      (Seq(0,3), Seq(2,4)),
      (Seq(1,2), Seq(3,2)),
      (Seq(2), Seq(1)),
      (Seq(1), Seq()))

    it("should have five nodes") { assert(graph.size === expectedGraph.length ) }
    it("should be isomorphic to the expected graph") {
      for ( (block, (preds, succs)) <- graph zip expectedGraph) {
    	  assert ( (block.getPreds().asScala map { _.getIndexInMethod() }) === preds)
    	  assert ( ( block.getSuccs().asScala  map { _.getIndexInMethod() }) === succs)
      }
    }
  }
}
