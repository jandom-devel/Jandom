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
package ui.cli
import it.unich.sci.jandom.targets.slil.SLILStmt
import it.unich.sci.jandom.narrowings.DelayedNarrowingFactory
import it.unich.sci.jandom.narrowings.NoNarrowing
import it.unich.sci.jandom.targets.slil.SLILStmt
import it.unich.sci.jandom.targets.slil.SLILStmt
import it.unich.sci.jandom.ppfactories.PPFactory.ConstantFactory

/**
 * A very minimalistic CLI.
 */
object JandomCLI extends App {
  val conf = new Conf(args)

  val source = scala.io.Source.fromFile(conf.file()).getLines.mkString("\n")
  val parsed = parsers.RandomParser().parseProgram(source)
  if (parsed.successful) {
    val program = parsed.get
    val domain = domains.PPLCPolyhedron
    val params = new targets.Parameters(domain, program: SLILStmt)
    params.narrowingStrategy = conf.narrowingStrategy()
    params.wideningScope = conf.wideningScope()        
    params.narrowingFactory = DelayedNarrowingFactory(NoNarrowing,2)
    val ann = program.analyze(params)
    println(program.mkString(ann))
  } else {
    println(parsed)
  }
}
