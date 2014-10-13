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

package it.unich.jandom

import it.unich.jandom.narrowings.DefaultNarrowing
import it.unich.jandom.ppfactories.DelayedNarrowingFactory
import it.unich.jandom.ppfactories.DelayedWideningFactory
import it.unich.jandom.ppfactories.MemoizingFactory
import it.unich.jandom.ppfactories.PPFactory.ConstantFactory
import it.unich.jandom.targets.NarrowingStrategy
import it.unich.jandom.targets.WideningScope
import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.targets.slil.SLILTarget
import it.unich.jandom.widenings.DefaultWidening

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
    val parsed = parsers.LPInvParser().parseProgram(source)
    if (parsed.successful) {
      val program = parsed.get
      val params = new targets.Parameters[LTS] { val domain = domains.numerical.BoxDoubleDomain() }
      val x= DelayedWideningFactory[LTS](DefaultWidening, 2)
      params.wideningFactory = MemoizingFactory(program)(DelayedWideningFactory(DefaultWidening, 2))
      params.narrowingFactory = MemoizingFactory(program)(DelayedNarrowingFactory(DefaultNarrowing, 2))
      println(program)
      val ann = program.analyze(params)
      println(ann)
    } else {
      println(parsed)
    }
  }
}
