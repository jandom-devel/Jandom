/**
  * Copyright 2015, 2016, 2017 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.benchmark

import java.nio.file._

import it.unich.jandom.parsers.RandomParser
import it.unich.jandom.targets.slil.SLILProgram
import it.unich.jandom.utils.ResourceWalker

/**
  * This trait loads and parser all programs in the /random resource directory.
  *
  * @author Gianluca Amato <gamato@unich.it>
  */
trait SLILLoader {

  private val resources = ResourceWalker.list("/random")

  /**
    * A sequence of SLIL program with corresponding file name.
    */
  val slils: Seq[(String, SLILProgram)] = for (r <- resources) yield {
    val stream = Files.newBufferedReader(r)
    (r.getFileName.toString, RandomParser().parse(stream).get)
  }

}
