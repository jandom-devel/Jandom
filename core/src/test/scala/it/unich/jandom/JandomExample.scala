/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom

import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.targets.parameters.NarrowingStrategy
import it.unich.jandom.targets.parameters.NarrowingSpecs._
import it.unich.jandom.targets.parameters.WideningScope
import it.unich.jandom.targets.parameters.WideningSpecs._
import it.unich.jandom.targets.slil.SLILTarget

/**
 * Example program using ''Jandom''.
 * @todo remove from ''Jandom'' and put into a related project.
 */
object JandomExample extends App {

  {
    val source = scala.io.Source.fromFile("examples/WideningPaper/nested.R").getLines.mkString("\n")
    val parsed = parsers.RandomParser().parseProgram(source)
    if (parsed.successful) {
      val program = parsed.get
      val params = new targets.Parameters[SLILTarget] { val domain = domains.numerical.BoxDoubleDomain() }
      params.narrowingStrategy = NarrowingStrategy.Restart
      params.wideningScope = WideningScope.BackEdges
      val ann = program.analyze(params)
      println(program.mkString(ann))
    } else {
      println(parsed)
    }
  }

  {
    val source = scala.io.Source.fromFile("examples/LPinv/berkeley.in").getLines.mkString("\n")
    val parsed = parsers.LPInvParser().parse("berkeley",source)
    if (parsed.successful) {
      val program = parsed.get
      val params = new targets.Parameters[LTS] { val domain = domains.numerical.BoxDoubleDomain() }
      params.widening = DelayedWidening(DefaultWidening, 2)
      params.narrowing = DelayedNarrowing(DefaultNarrowing, 2)
      println(program)
      val ann = program.analyze(params)
      println(ann)
    } else {
      println(parsed)
    }
  }
}
