/**
 * Copyright 2013 Gianluca Amato
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

import scala.collection.mutable.ArrayStack
import it.unich.sci.jandom.targets.linearcondition.AtomicCond
import it.unich.sci.jandom.targets.LinearForm
import it.unich.sci.jandom.widenings.Widening
import it.unich.sci.jandom.narrowings.Narrowing
import it.unich.sci.jandom.domains.numerical.NumericalProperty
import it.unich.sci.jandom.domains.numerical.NumericalDomain

/**
 * This is an abstract JVM environment using a fixed frame and stack. At the moment, it only supports
 * numerical properties.
 * @tparam Property the numerical property used to describe numerical variables
 * @param maxLocals the mumber of locals in the frame
 * @param property the numerical property relating all numerical and stack positions
 * @author Gianluca Amato <gamato@unich.it>
 */
class JVMEnvFixedFrame[NumProperty <: NumericalProperty[NumProperty]](
  val maxLocals: Int, var property: NumProperty) extends JVMEnv[JVMEnvFixedFrame[NumProperty]] {

  override def clone: JVMEnvFixedFrame[NumProperty] = new JVMEnvFixedFrame(maxLocals, property)

  def empty {
    property = property.empty
  }

  def ipush(c: Int) {
    property = property.addDimension.constantAssignment(property.dimension, c)
  }

  def istore(v: Int) {
    property = property.variableAssignment(v, property.dimension - 1).delDimension(property.dimension - 1)
  }

  def iload(v: Int) {
    property = property.addDimension.variableAssignment(property.dimension, v)
  }

  def iadd() {
    property = property.variableAdd(property.dimension - 2, property.dimension - 1).delDimension(property.dimension - 1)
  }

  def iinc(v: Int, c: Int) {
    property = property.constantAdd(v, c)
  }

  def if_icmp(op: AtomicCond.ComparisonOperators.Value) {
    import AtomicCond.ComparisonOperators._
    val lfm = LinearForm.fromVar[Int](property.dimension - 1 + 1)
    val lfn = LinearForm.fromVar[Int](property.dimension - 2 + 1)
    val condition = op match {
      case LT => AtomicCond(lfn - lfm + LinearForm.fromCoefficient(1), LTE)
      case GT => AtomicCond(lfn - lfm - LinearForm.fromCoefficient(1), GTE)
      // TODO we should optmized NEQ
      case _ => AtomicCond(lfn - lfm, op)
    }
    property = condition.analyze(property).delDimension(property.dimension - 1).delDimension(property.dimension - 2)
  }

  def union(that: JVMEnvFixedFrame[NumProperty]): JVMEnvFixedFrame[NumProperty] =
    new JVMEnvFixedFrame[NumProperty](maxLocals, property union that.property)

  def intersection(that: JVMEnvFixedFrame[NumProperty]): JVMEnvFixedFrame[NumProperty] =
    new JVMEnvFixedFrame[NumProperty](maxLocals, property intersection that.property)

  def narrowing(that: JVMEnvFixedFrame[NumProperty]): JVMEnvFixedFrame[NumProperty] =
    new JVMEnvFixedFrame[NumProperty](maxLocals, property narrowing that.property)

  def widening(that: JVMEnvFixedFrame[NumProperty]): JVMEnvFixedFrame[NumProperty] =
    new JVMEnvFixedFrame[NumProperty](maxLocals, property widening that.property)

  def tryCompareTo[B >: JVMEnvFixedFrame[NumProperty]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: JVMEnvFixedFrame[NumProperty] =>
      property.tryCompareTo(other.property)
    case _ => None
  }

  override def equals(that: Any) =  that match {
    case that: JVMEnvFixedFrame[NumProperty] => property==that.property && maxLocals == that.maxLocals
    case _ => false
  }

  def mkString(vars: IndexedSeq[String]) =
    property.mkString(vars ++ ((maxLocals until property.dimension) map { i => "s" + i }))

  override def toString =
    mkString((0 until maxLocals) map { i => "i" + i }).mkString(",")
}

/**
 * This is the abstract domain of JVM environments witrh fixed frames. At the moment, it only deals with numerical
 * properties.
 * @param dom the numerical domain to use for the numerical variables.
 * @author Gianluca Amato <gamato@unich.it>
 */
class JVMEnvFixedFrameDomain(val dom: NumericalDomain) extends JVMEnvDomain {
  type Property = JVMEnvFixedFrame[dom.Property]

  def full(maxLocals: Int) = new JVMEnvFixedFrame[dom.Property](maxLocals, dom.full(maxLocals))

  def empty(maxLocals: Int) = new JVMEnvFixedFrame[dom.Property](maxLocals, dom.empty(maxLocals))

}
