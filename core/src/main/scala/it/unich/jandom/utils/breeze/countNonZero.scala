/**
 * Copyright 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.utils.breeze

import breeze.generic.UFunc
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor

object countNonZero extends UFunc {
  implicit def countFromTraverseDoubles[T](implicit traverse: CanTraverseValues[T, Double]): Impl[T, Int] = {
    new Impl[T, Int] {
      def apply(t: T): Int = {
        var count: Int = 0
        traverse.traverse(t, new ValuesVisitor[Double] {
          def visit(a: Double) = { if (a != 0) count += 1 }
          def zeros(count: Int, zeroValue: Double) {}
        })
        count
      }
    }
  }
}
