/**
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
 *
 * (c) 2011 Gianluca Amato
 */

package it.unich.sci.jandom

import parma_polyhedra_library.{Parma_Polyhedra_Library => PPL}
import domains.PPLCPolyhedron
import targets.slil.SLILProgram
import targets.lts.LTS
import domains._
import widenings._
import narrowings._
import ppfactories._
import annotations.BlackBoard
import it.unich.sci.jandom.targets.slil.SLILStmt

object Jandom extends App {

  System.load("/usr/local/lib/ppl/libppl_java.so")
  PPL.initialize_library()
 
  { 
    val source = scala.io.Source.fromFile("examples/Random/incr.R").getLines.mkString("\n")
    val parsed = parsers.RandomParser.parseProgram(source)  
    if (parsed.successful) {
	  val program = parsed.get 
	  val params = new targets.Parameters[domains.PPLBoxDouble,SLILStmt](domains.PPLBoxDouble,program)
      params.narrowingFactory = MemoizingFactory(DelayedNarrowingFactory(DefaultNarrowing,2),program)
      val ann = program.analyze(params)	  
      println(program.mkString(ann))
    } else {
      println(parsed)
    }
  }
  
  {
    val source = scala.io.Source.fromFile("examples/LPinv/berkeley.in").getLines.mkString("\n")
    val parsed = parsers.LPInvParser.parseProgram(source)  
    if (parsed.successful) {
   	  val program = parsed.get 
      val params = new targets.Parameters(domains.PPLCPolyhedron,program)
      params.wideningFactory = MemoizingFactory(DelayedWideningFactory(DefaultWidening,2),program)
   	  println(program)
   	  val ann = program.analyze(params)        
      println(ann)
    } else {
      println(parsed)
    }      
  }
  
  PPL.finalize_library()
}
