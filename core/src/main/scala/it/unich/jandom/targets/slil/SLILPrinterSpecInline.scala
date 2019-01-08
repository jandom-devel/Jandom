/**
  * Copyright 2013, 2018 Gianluca Amato
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

package it.unich.jandom.targets.slil

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.Environment

/**
  * This is a SLILPrinterSpec which prints the annotations inline.
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */
class SLILPrinterSpecInline(val env: Environment, val indentWidth: Int) extends SLILPrinterSpec {

  def decorator(p: NumericalProperty[_], row: Int, col: Int): String = p.mkString(env.variables)
}

/**
  * The companion object for PrettyPrinterSpecInline.
  */
object SLILPrinterSpecInline {

  /**
    * Builds a PrettyPrinterSpecInline.
    */
  def apply(env: Environment, indentWidth: Int = 2) = new SLILPrinterSpecInline(env, indentWidth)
}
