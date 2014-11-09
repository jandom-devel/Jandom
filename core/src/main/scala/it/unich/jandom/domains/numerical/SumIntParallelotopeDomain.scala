/**
 * Copyright 2014 Gianluca Amato, Francesca Scozzari, Simone Di Nardo Di Maio
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

package it.unich.jandom.domains.numerical

/**
 * This is the class which implements the abstract domain sum of intervals and parallelotopes.
 * @author Gianluca Amato
 * @author Francesca Scozzari
 * @author Simone Di Nardo Di Maio
 */

class SumIntParallelotopeDomain(val dom1: BoxDoubleDomain, val dom2: ParallelotopeDomain) extends SumDomain[BoxDoubleDomain, ParallelotopeDomain] {
  type Property = SumIntParallelotope

  class SumIntParallelotope(val p1: dom1.Property, val p2: dom2.Property) extends Sum {
    override def linearAssignment(n: Int, lf: LinearForm[Double]): Property = {
      val homlf = new DenseLinearForm[Double](0.0 +: lf.homcoeffs)
      if ((n >= lf.homcoeffs.size) || (lf.homcoeffs(n) == 0)) {
        val q1 = p1.linearAssignment(n, lf)
        val q2 = p2.linearAssignment(n, homlf)
        SumIntParallelotopeDomain.this(q1, q2)
      } else {
        val q1 = p1.linearAssignment(n, homlf)
        val q2 = p2.linearAssignment(n,lf)
        SumIntParallelotopeDomain.this(q1, q2)
      }
    }
  }

  def apply(p1: dom1.Property, p2: dom2.Property) = new SumIntParallelotope(p1, p2)
}

/**
 * Companion class for the Int+Parallelotope domain
 */
object SumIntParallelotopeDomain {
  /**
   *  Returns the standard Int+Parallelotope Domain
   */
  def apply() = v
  private lazy val v = new SumIntParallelotopeDomain(BoxDoubleDomain(), ParallelotopeDomain(favorAxes = true))
}
