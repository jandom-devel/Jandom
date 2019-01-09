/**
  * Copyright 2018 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.ui.output

import java.io.File

import org.apache.commons.text.StringEscapeUtils

import scala.collection.mutable
import scala.io.Source

class HTMLOutputBuilder(val indentSize: Int = 2) extends OutputBuilder {

  import IndentBehaviour._

  private val sb = new StringBuilder()

  private var indentLevel = 0

  private val lines = mutable.Buffer[String]()

  private val ann = mutable.Buffer[(Int, Int, String)]()

  def ++=(s: String): this.type = {
    sb ++= s
    this
  }

  def annotate(s: String): this.type = {
    ann += ((lines.size, sb.size, s))
    this
  }

  def newline(indent: IndentBehaviour = Same): this.type = {
    lines += '"' + StringEscapeUtils.escapeEcmaScript(sb.toString()) + '"'
    sb.clear()
    indent match {
      case Increase => indentLevel += indentSize
      case Decrease => indentLevel -= indentSize
      case _ =>
    }
    sb ++= " " * indentLevel
    this
  }

  override def toString: String = {
    val htmlFile = new File(getClass.getResource("/htmlout/index.html").toURI)
    val html = Source.fromFile(htmlFile).getLines.mkString("\n")
    val jsonAnn = for ((row, col, s) <- ann) yield s"""{ row: $row, col: $col, note: "${StringEscapeUtils.escapeEcmaScript(s)}" }"""
    html.replace("%%PROG%%", lines.mkString(",")).replace("%%ANN%%", jsonAnn.mkString(",\n"))
  }
}

object HTMLOutputBuilder {
  def apply(indentSize: Int = 2): HTMLOutputBuilder = new HTMLOutputBuilder(indentSize)
}
