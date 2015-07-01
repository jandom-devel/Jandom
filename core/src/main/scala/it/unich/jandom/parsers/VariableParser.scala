/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>, Francesca Scozzari <fscozzari@unich.it>
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

package it.unich.jandom.parsers

import scala.util.parsing.combinator.JavaTokenParsers

import it.unich.jandom.targets.Environment

trait VariableParser extends JavaTokenParsers {
  
  /**
   * The environment to use for parsing variable names.
   */
  val env: Environment
  
  /**
   * If this variable is true, unrecognized variables will be treated as errors, otherwise they will be
   * added to the environment.
   */
  var closedVariables: Boolean = false

  /**
   * Parser for variables.
   */
  val variable: Parser[Int] = new Parser[Int] {
    def apply(in: Input) = ident(in) match {
      case Success(i, in1) => env.getBinding(i) match {
        case Some(v) => Success(v, in1)
        case None => if (closedVariables) Failure("Unexpected variable", in1) else Success(env.addBinding(i), in1)
      }
      case default => default.asInstanceOf[ParseResult[Int]]
    }
  }
  
  val parameterVariable: Parser[Int] = new Parser[Int] {
    def apply(in: Input) = ident(in) match {
      case Success(i, in1) => env.getBinding("@"+i) match {
        case Some(v) => Success(v, in1)
        case None => if (closedVariables) Failure("Unexpected variable", in1) else Success(env.addBinding(i), in1)
      }
      case default => default.asInstanceOf[ParseResult[Int]]
    }
  }
  
}
