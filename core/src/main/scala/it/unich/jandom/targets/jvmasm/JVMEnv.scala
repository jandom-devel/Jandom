/**
 * Copyright 2013 amato
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

package it.unich.jandom.targets.jvmasm

import it.unich.jandom.domains.AbstractDomain
import it.unich.jandom.narrowings.Narrowing
import it.unich.jandom.widenings.Widening
import it.unich.jandom.domains.AbstractProperty
import it.unich.jandom.targets.NumericCondition._

/**
 * This is the base class for abstractions of the JVM environment. It uses F-bounded polymorhpism to
 * be completely typesafe. It implements a mutable interface.
 * @tparam Property the property which is to be considered a JVM environment
 * @author Gianluca Amato
 *
 */
abstract class JVMEnv[Property <: JVMEnv[Property]] extends AbstractProperty[Property] with Cloneable {

  /**
   * Returns a deep copy of JVMEnv.
   */
  override def clone: Property = super.clone.asInstanceOf[Property]

  /**
   * Empties the abstract environment (i.e., it returns an abstract environment
   * representing no concrete environments).
   */
  def empty()

  def ipush(c: Int)

  def istore(v: Int)

  def iload(v: Int)

  def iadd()

  def iinc(v: Int, c: Int)

  def if_icmp(op: ComparisonOperators.Value)

}

/**
 * This is the base class for domains abstracting the JVM Environment.
 */
abstract class JVMEnvDomain extends AbstractDomain {
  type Property <: JVMEnv[Property]

  /**
   * Returns the top elements for the given number of locals
   * @param numLocal number of locals in the frame
   */
  def full(numLocals: Int): Property
}
