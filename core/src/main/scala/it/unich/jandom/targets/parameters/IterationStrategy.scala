/**
 * Copyright 2014, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.targets.parameters

/**
 * This parameter specifies which kind of iterations strategy to use for the analysis:
 * - Kleene: compute all state transformers at the same time (Kleene non-chaotic iteration)
 * - WorkList: simple worklist-based iteration strategy
 * At the moment, this is only supported by the LTS target.
 */
object IterationStrategy extends Enumeration {
  type IterationStrategy = Value

  val Kleene = Value
  val Worklist = Value
}
