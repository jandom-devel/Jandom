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

package it.unich.sci.jandom.domains.numerical

/**
 * This is the class for numerical properties of the top domain. There is a single `NumericalTop` for each
 * environment dimension.
 * @param dimension number of dimensions of the environment
 */
case class NumericalTop (val dimension: Int) extends NumericalProperty[NumericalTop] {
  def tryCompareTo[B >: NumericalTop](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: NumericalTop => Some(0)
    case _ => None
  }
  def widening(that: NumericalTop) = this
  def union(that: NumericalTop) = this
  def narrowing(that: NumericalTop) = this
  def intersection(that: NumericalTop) = this
  def nonDeterministicAssignment(n: Int) = this
  def linearAssignment(n:Int, coeff:Array[Double], known: Double) = this
  def linearInequality(coeff: Array[Double], known: Double) = this
  def linearDisequality(coeff: Array[Double], known: Double) = this
  def minimize(coeff: Array[Double], known: Double) = Double.NegativeInfinity
  def maximize(coeff: Array[Double], known: Double) = Double.PositiveInfinity
  def frequency(coeff: Array[Double], known: Double) = None
  def addDimension = NumericalTopDomain.full(dimension+1)
  def delDimension(n: Int = dimension-1) = NumericalTopDomain.full(dimension-1)
  def mapDimensions(rho: Seq[Int]) = NumericalTopDomain.full(rho count { _ != -1 })
  def isEmpty = false
  def isFull = true
  def empty = throw new UnsupportedOperationException()
  def full = this
  def mkString(vars: IndexedSeq[String]): Seq[String] = Seq("top")
}

/**
 * The most abstract numerical domain.
 * @author Gianluca Amato <gamato@unich.it>
 */
object NumericalTopDomain extends NumericalDomain {
  type Property = NumericalTop
  def full(n: Int) = new NumericalTop(n)
  def empty(n: Int) = new NumericalTop(n)
}