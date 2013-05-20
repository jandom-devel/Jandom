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

import org.scalatest.FunSpec
import soot._

/**
 * Test suite for the [[soot.Jandom.BriefBigBlockGraph]].
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
class BriefBigBlockGraphSuite extends FunSpec {
  val scene = Scene.v()
  scene.setSootClassPath(scene.defaultClassPath + ":examples/Java/")
  val c = scene.loadClass("SimpleTest", 1)
  c.setApplicationClass()

  describe("The BriefBigBlockGraph for the sequential method") {
    val body = c.getMethodByName("sequential").retrieveActiveBody()
    val graph = new BriefBigBlockGraph(body)
    val units = body.getUnits()
    val block = graph.getBlocks().get(0)

    it("should have a single node") { assert(graph.size() === 1) }

    describe("Its only node") {
      it("should have no successors") { assert(block.getSuccs().isEmpty()) }
      it("should have predecessors") { assert(block.getSuccs().isEmpty()) }
      it("should have head equal to the first unit in the body") { assert(block.getHead() === units.getFirst()) }
      it("shoudl have tail equal to the last unit in the body") { assert(block.getTail() === units.getLast()) }
    }
  }

  describe("The BriefBigBlockGraph for the nested method") {
    import scala.collection.JavaConversions._

    val body = c.getMethodByName("nested").retrieveActiveBody()
    val graph = new BriefBigBlockGraph(body)
    val expectedGraph = Seq(
      (Seq(), Seq(1)),
      (Seq(0, 3), Seq(2, 4)),
      (Seq(1, 2), Seq(3, 2)),
      (Seq(2), Seq(1)),
      (Seq(1), Seq()))

    it("should have five nodes") { assert(graph.size() === 5) }
    it("should be isomorphic to the expected graph") {
      for ( (block, (preds, succs)) <- graph zip expectedGraph) {
    	  assert ( (block.getPreds() map { _.getIndexInMethod() }) === preds)
    	  assert ( ( block.getSuccs()  map { _.getIndexInMethod() }) === succs)
      }
    }
  }
}