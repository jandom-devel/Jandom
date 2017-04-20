/**
 * Copyright 2014, 2016 Gianluca Amato, Francesca Scozzari, Simone Di Nardo Di Maio
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

package it.unich.jandom.domains.numerical

/**
 * This is the class which implements the abstract domain sum of intervals and parallelotopes.
 * @author Gianluca Amato
 * @author Francesca Scozzari
 * @author Simone Di Nardo Di Maio
 */

class SumBoxDoubleParallelotopeRationDomain(val dom1: BoxDoubleDomain, val dom2: ParallelotopeRationalDomain) extends SumDomain[BoxDoubleDomain, ParallelotopeRationalDomain] {
  type Property = SumBoxDoubleParallelotopeRational

  class SumBoxDoubleParallelotopeRational(val p1: dom1.Property, val p2: dom2.Property) extends Sum {
    override def linearAssignment(n: Int, lf: LinearForm): Property = {
      if ((n >= lf.homcoeffs.size) || (lf.homcoeffs(n) == 0)) {
        val q1 = p1.linearAssignment(n, lf)
        val q2 = p2.linearAssignment(n, lf.hom)
        SumBoxDoubleParallelotopeRationDomain.this(q1, q2)
      } else {
        val q1 = p1.linearAssignment(n, lf.hom)
        val q2 = p2.linearAssignment(n,lf)
        SumBoxDoubleParallelotopeRationDomain.this(q1, q2)
      }
    }
  }

  def apply(p1: dom1.Property, p2: dom2.Property) = new SumBoxDoubleParallelotopeRational(p1, p2)
}

/**
 * Companion class for the Int+Parallelotope domain
 */
object SumBoxDoubleParallelotopeRationDomain {
  /**
   *  Returns the standard Int+Parallelotope Domain
   */
  def apply() = v
  private lazy val v = new SumBoxDoubleParallelotopeRationDomain(BoxDoubleDomain(), ParallelotopeRationalDomain(favorAxis = 1))
}
