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

/**
  * An output builder is a class which helps producing line-based text output with embedded annotations.
  */
abstract class OutputBuilder {

  /**
    * An enumeration type with possible indentation behaviour when add a newline. The possible values are:
    * - Same: same indentation as the current line
    * - Increase: increase indentation level w.r.t. current line
    * - Decrease: decrease indentation level w.r.t. current line
    */
  object IndentBehaviour extends Enumeration {
    type IndentBehaviour = Value
    val Same, Increase, Decrease = Value
  }

  import IndentBehaviour._

  /**
    * Add a string to the output. The string is supposed not to contain newlines.
    *
    * @param s string to add in the output
    * @return the same object as `this`, to simplify chaining
    */
  def ++=(s: String): this.type

  /**
    * Add a string to the output as an annotation. The string is supposed not to contain newlines.
    *
    * @param s string to add in the output
    * @return the same object as `this`, to simplify chaining
    */
  def annotate(s: String): this.type

  /**
    * Add a newline to the output and select the indentation for the next line.
    *
    * @param indent indentation for the next line
    * @return the same object as `this`, to simplify chaining
    */
  def newline(indent: IndentBehaviour = Same): this.type

  /**
    * Returns the output accumulated in the builder as a string.
    */
  def toString: String
}
