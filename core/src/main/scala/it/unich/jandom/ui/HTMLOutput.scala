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

package it.unich.jandom.ui

import java.io.File

import scala.io.Source

/**
 * This object takes a string and a set of annotations (with line and column numbers) and generates an
 * HTML file to represents the result. The template of the HTML is the resource /htmlout/index.html.
 * The strings '%%PROG%%' e '%%ANN%%' are replaced with the program and annotation respectively.
 */
object HTMLOutput {
  val htmlFile = new File(getClass().getResource("/htmlout/index.html").toURI)
  val html = Source.fromFile(htmlFile).getLines.mkString("\n")

  def apply(out: String, ann: Seq[(Int,Int,String)]): String = {
    val prog = out.split('\n').map { '"' + _ + '"' }
    val jsonAnn = for ((row, col, s) <- ann) yield s"""{ row: ${row}, col: ${col}, note: "${s}" }"""

    html.replace("%%PROG%%", prog.mkString(",")).replace("%%ANN%%",jsonAnn.mkString(",\n"))
  }

}
