/**
 * Copyright 2013 Gianluca Amato
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

package it.unich.jandom.parsers

import java.io.File
import java.io.FilenameFilter

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.targets.Environment
import it.unich.jandom.targets.linearcondition.AtomicCond
import it.unich.jandom.targets.slil._

/**
 * Test suite for RandomParser.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class RandomParserSuite extends FunSuite with Checkers {
  test("very simple random program") {
    val prog: String = """
      xyz <- function(x,y) x = 1
    """
    val env = Environment("x", "y")
    val program = SLILProgram(env, Seq(0, 1), CompoundStmt(AssignStmt(0, LinearForm(1, 0, 0))))
    val parsed = RandomParser().parseProgram(prog).get
    assertResult(program) { parsed }
  }

  test("simple random program") {
    val prog: String = """
       xyline <- function(x) {
          y = 0;
          while (y < x)
    		y=y+1
      }
    """
    val env = Environment("x", "y")
    val program = SLILProgram(env, List(0),
      CompoundStmt(
        AssignStmt(1, LinearForm(0, 0 -> 1)),
        WhileStmt(AtomicCond(LinearForm(0, -1, 1), AtomicCond.ComparisonOperators.LT),
          CompoundStmt(AssignStmt(1, LinearForm(1, 0, 1))))))
    assertResult(program) { RandomParser().parseProgram(prog).get }
  }

  val resourceURL = getClass().getResource("/random")
  val dir = new File(resourceURL.toURI())

  val nameFilter = new FilenameFilter() {
    def accept(dir: File, name: String) = name.endsWith(".R");
  }
  val files = dir.listFiles(nameFilter)
  for (f <- files) {
    test("SLIL Target analyze "+f.getName()) {
      val source = scala.io.Source.fromFile(f).getLines.mkString("\n")
      val parser = RandomParser()
      parser.parseProgram(source) match {
        case parser.Success(_,_) =>
        case parser.NoSuccess(msg, next) => fail(msg + " in line " + next.pos.line + " column " + next.pos.column)
      }
    }
  }
}
