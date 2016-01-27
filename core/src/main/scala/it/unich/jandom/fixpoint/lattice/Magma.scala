/**
 * Copyright 2015 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.fixpoint.lattice

import scala.language.implicitConversions

/**
 * A Magma is a set with a binary operation (op)
 */
trait Magma[A] {
  def op(x: A, y: A): A
}

object Magma {
  implicit def magmaOps[A: Magma](a: A): MagmaOps[A] = new MagmaOps(a)

  final class MagmaOps[A](val lhs: A)(implicit ev: Magma[A]) {
    def |+|(rhs: A): A = ev.op(lhs, rhs)
  }
}
