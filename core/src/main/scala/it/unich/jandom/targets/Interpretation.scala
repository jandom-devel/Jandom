/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.targets

import soot.SootMethod

/**
 * This is the common trait for all interpretations.
 * An interpretation is a map from methods, function or procedures to their corresponding semantics.
 * It is used in the inter-procedural analysis of programs. For the moment, it is only supported
 * by the SootCFG targets. The API is fundamentally broken and should be reviewed.
 * @tparam Tgt the target under analysis
 * @tparam Params the parameter for the analysis
 * @author Gianluca Amato <gamato@unich.it>
 */

trait Interpretation[Tgt <: Target[Tgt], Params <: Parameters[Tgt]] {
  /**
   * Each interpretation is associated with some parameters for analysis. Parameters are used
   * for determining input and output types of the analysis.
   */
  val params: Params

  /* TODO generalize the API replacing `SootMethod` with something dependent from `Tgt`. Moreover, the
     fibers of input and of the return value should be related in some way with the method. */
  /**
   * An interpretation is a map that, given a method and an input, returns the semantics of the
   * method on the given input.
   * @param method it is `SootMethod`. This is wrong, this type should depend from `Tgt`, but the
   * API is not yet general
   * @param input the input property to the method
   * @param output the output property according to method semantics
   */
   def apply(method: SootMethod, input: params.Property): params.Property
}
