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

package it.unich.sci.jandom.targets.jvm

import it.unich.sci.jandom.domains.AbstractDomain
import it.unich.sci.jandom.targets.linearcondition.AtomicCond
import it.unich.sci.jandom.narrowings.Narrowing
import it.unich.sci.jandom.widenings.Widening

/**
 * This is the base class for abstractions of the JVM environment. It uses F-bounded polymorhpism to
 * be completely typesafe. It implements a mutable interface.
 * @tparam Property the property which is to be considered a JVM environment
 * @author Gianluca Amato
 *
 */
abstract class JVMEnv[Property <: AnyRef] extends Cloneable {
  def empty

  def ipush(c: Int)

  def istore(v: Int)

  def iload(v: Int)

  def iadd()

  def iinc(v: Int, c: Int)

  def if_icmp(op: AtomicCond.ComparisonOperators.Value)

  def union(that: Property): Boolean

  def intersection(that: Property): Boolean

  def narrowing(that: Property, n: Narrowing): Boolean

  def widening(that: Property, w: Widening): Boolean

  def mkString(vars: IndexedSeq[String]): String

  override def clone: Property = super.clone.asInstanceOf[Property]
}

/**
 * This is the base class for domains abstracting the JVM Environment.
 */
abstract class JVMEnvDomain extends AbstractDomain {

  type Property <: JVMEnv[Property]

  /**
   * Creates a full JVM environment.
   * @param maxLocal maximum number of locals in the frame.
   */
  def full(maxLocals: Int): Property

  /**
   * Creates an empty JVM environment.
   * @param maxLocal maximum number of locals in the frame.
   */

  def empty(maxLocals: Int): Property
}
