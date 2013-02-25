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

package it.unich.sci.jandom

import it.unich.sci.jandom.ppfactories.PPFactory.ConstantFactory
import domains.PPLCPolyhedron
import narrowings.{DefaultNarrowing, DelayedNarrowingFactory}
import parma_polyhedra_library.Parma_Polyhedra_Library
import ppfactories.MemoizingFactory
import targets.slil.SLILStmt
import widenings.{DefaultWidening, DelayedWideningFactory}

/**
 * Example program using ''Jandom''.
 * @todo remove from ''Jandom'' and put into a related project.
 */
object JandomExample extends App {

  {
    val source = scala.io.Source.fromFile("examples/nested.R").getLines.mkString("\n")
    val parsed = parsers.RandomParser().parseProgram(source)
    if (parsed.successful) {
      val program = parsed.get
      val domain =  domains.BoxDouble
      val params = new targets.Parameters(domain, program: SLILStmt)
      params.narrowingStrategy = it.unich.sci.jandom.ui.NarrowingStrategy.Restart
      params.wideningScope = it.unich.sci.jandom.ui.WideningScope.BackEdges
      val ann = program.analyze(params)
      println(program.mkString(ann))
    } else {
      println(parsed)
    }
  }

  {
    val source = scala.io.Source.fromFile("examples/LPinv/berkeley.in").getLines.mkString("\n")
    val parsed = parsers.LPInvParser().parseProgram(source)
    if (parsed.successful) {
      val program = parsed.get
      val params = new targets.Parameters(PPLCPolyhedron, program)
      params.wideningFactory = MemoizingFactory(DelayedWideningFactory(DefaultWidening, 2), program)
      params.narrowingFactory = MemoizingFactory(DelayedNarrowingFactory(DefaultNarrowing, 2), program)
      println(program)
      val ann = program.analyze(params)
      println(ann)
    } else {
      println(parsed)
    }
  }
  println("Jandom version: "+version)
  println("PPL version: "+Parma_Polyhedra_Library.version);  
}
