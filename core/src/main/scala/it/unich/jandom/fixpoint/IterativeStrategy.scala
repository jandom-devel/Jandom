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

package it.unich.jandom.fixpoint

/**
 * An IterativeStrategy is a hierarchical ordering, according to Bourdoncle definition in the paper
 * "Efficient chaotic iteration strategies with widenings", FMPA'93.
 * @tparam U the type of the elements to order
 * @param seq a sequence of elements `Left`, `Right`, and `El(u)` of type `IterativeStrategy.StrategyElement[U]`
 * which represents the ordering. No check is done that the hierarchical ordering satisfy the fundamental property
 * that there are no two consecutive left parentheses.
 */
class IterativeStrategy[U] private (val seq: IndexedSeq[IterativeStrategy.StrategyElement[U]]) extends AnyVal {
  /**
   * Returns the i-th element of the sequence.
   */
  def apply(i: Int) = seq(i)

  /**
   * Returns the number of the elements in the ordering.
   */
  def length = seq.length
}

object IterativeStrategy {
  /**
   * A StrategyElement[U] is either `Left` (left parenthesis), `Right` (right parenthesis) or
   * `El(u)` where `u` is a value of type `U`.
   */
  sealed abstract class StrategyElement[+U]
  case object Left extends StrategyElement[Nothing]
  case object Right extends StrategyElement[Nothing]
  case class El[U](val u: U) extends StrategyElement[U]

  /**
   * Builds an iterative strategy from a sequence of `StrategyElement`.
   */
  def apply[U](els: StrategyElement[U]*) = new IterativeStrategy(els.toIndexedSeq)
}
