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
import it.unich.sci.jandom.domains.{ AbstractDomain, NumericalDomain, NumericalProperty }
import it.unich.sci.jandom.targets.linearcondition.AtomicCond
import it.unich.sci.jandom.targets.LinearForm
import it.unich.sci.jandom.widenings.Widening
import it.unich.sci.jandom.narrowings.Narrowing

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

  def union(that: JVMEnvFixedFrame[NumProperty]): Boolean = {
    val oldproperty = property
    property = property union that.property
    if (property > oldproperty)
      true
    else
      false
  }

  def intersection(that: JVMEnvFixedFrame[NumProperty]): Boolean = {
    val oldproperty = property
    property = property intersection that.property
    if (property < oldproperty)
      true
    else
      false
  }

  def narrowing(that: JVMEnvFixedFrame[NumProperty], n: Narrowing): Boolean = {
    val oldproperty = property
    property = n(property, that.property)
    if (property < oldproperty)
      true
    else
      false
  }


  def widening(that: JVMEnvFixedFrame[NumProperty], w: Widening): Boolean = {
    val oldproperty = property
    property = w(property, that.property)
    if (property > oldproperty)
      true
    else
      false
  }

  def tryCompareTo[B >: JVMEnvFixedFrame[NumProperty]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: JVMEnvFixedFrame[NumProperty] =>
    	property.tryCompareTo(other.property)
    case _ => None
  }

  def mkString(vars: IndexedSeq[String]) = property.mkString(vars).mkString(", ")

  override def toString = {
    val vars = ((0 until maxLocals) map { i => "i" + i }) ++
      ((maxLocals until property.dimension) map { i => "s" + i })
    mkString(vars)
  }
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
