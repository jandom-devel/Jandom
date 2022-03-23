/**
 * Copyright 2013, 2016 Gianluca Amato
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

package it.unich.jandom.targets.jvmasm

import scala.collection.mutable.Stack

import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.NumericCondition._
import spire.math.Rational

/**
 * This is an abstract JVM environment using a dynamically expandable frame. At the moment, it only supports
 * numerical properties.
 * @tparam NumProperty the numerical property used to describe numerical variables
 * @param frame associates each element on the JVM frame to a dimension of `property`. The value `-1`
 * corresponds to a non-numerical local variable.
 * @param stack associates each element on the JVM stack to a dimension of `property`.  The value `-1`
 * corresponds to a non-numerical stack position.
 * @param property the numerical property describing the state of numerical variables.
 * @author Gianluca Amato <gamato@unich.it>
 */

class JVMEnvDynFrame[NumProperty <: NumericalProperty[NumProperty]](
  val domain: JVMEnvDynFrameDomain, val frame: Array[Int], val stack: Stack[Int], var property: NumProperty) extends JVMEnv[JVMEnvDynFrame[NumProperty]] {

  type Domain = JVMEnvDynFrameDomain

  override def clone: JVMEnvDynFrame[NumProperty] =
    new JVMEnvDynFrame(domain,frame.clone, stack.clone, property)

  /**
   * Remove a dimension in the numerical property and auxiliary structures. The hypothesis
   * is that `n` does not occur in `frame` and `stack`.
   */
  private def delDimension(n: Int) = {
    frame mapInPlace { j => if (j > n) j - 1 else j }
    stack mapInPlace { j => if (j > n) j - 1 else j }
    property = property.delVariable(n)
  }

  /**
   * Return the numerical property of `this` with dimensions conformant with `that`
   */
  private def propertyConformantWith(that: JVMEnvDynFrame[NumProperty]): NumProperty = {
    // TODO fix mapDimension in numerical property so that it can also add dimensions. In this way, we do not need this complexity.
    var extractedProperty = this.property
    var dimMap = scala.collection.mutable.IndexedSeq.fill(extractedProperty.dimension)(-1)
    for ((o, n) <- (frame zip that.frame))
      if (o != -1)
        dimMap(o) = n
      else if (o == -1 && n != -1) {
        extractedProperty = extractedProperty.addVariable()
        dimMap = dimMap :+ n
      }
    for ((o, n) <- (stack zip that.stack)) dimMap(o) = n
    // TODO: change parameters of mapVariables in order to allow for mutable sequences
    extractedProperty.mapVariables(dimMap.toSeq)
  }

  def empty() = {
    property = property.bottom
  }

  def ipush(c: Int) = {
    val newdim = property.dimension
    property = property.addVariable().constantAssignment(newdim, c)
    stack.push(newdim)
  }

  def istore(v: Int) = {
    val oldn = frame(v)
    frame(v) = stack.pop()
    if (oldn != -1) delDimension(oldn)
  }

  def iload(v: Int) = {
    val vn = frame(v)
    val newdim = property.dimension
    property = property.addVariable().variableAssignment(newdim, vn)
    stack.push(newdim)
  }

  def iadd() = {
    val vm = stack.pop()
    val vn = stack.top
    property = property.variableAdd(vn, vm)
    delDimension(vm)
  }

  def iinc(v: Int, c: Int) = {
    val vn = frame(v)
    property = property.constantAdd(vn, 1)
  }

  def if_icmp(op: ComparisonOperators.Value) = {
    import ComparisonOperators._
    val vm = stack.pop()
    val vn = stack.pop()
    val lfm = LinearForm.v(vm)
    val lfn = LinearForm.v(vn)
    val condition = op match {
      case LT => AtomicCond(lfn - lfm + Rational.one, LTE)
      case GT => AtomicCond(lfn - lfm - Rational.one, GTE)
      // TODO optimize NEQ
      case _ => AtomicCond(lfn - lfm, op)
    }
    property = condition.analyze(property)
    delDimension(vm max vn)
    delDimension(vm min vn)
  }

  def union(that: JVMEnvDynFrame[NumProperty]) =
    new JVMEnvDynFrame(domain, frame.clone, stack.clone, property union that.propertyConformantWith(this))

  def intersection(that: JVMEnvDynFrame[NumProperty]): JVMEnvDynFrame[NumProperty] =
    new JVMEnvDynFrame(domain, frame.clone, stack.clone, property intersection that.propertyConformantWith(this))

  def narrowing(that: JVMEnvDynFrame[NumProperty]): JVMEnvDynFrame[NumProperty] =
    new JVMEnvDynFrame(domain, frame.clone, stack.clone, property narrowing that.propertyConformantWith(this))

  def widening(that: JVMEnvDynFrame[NumProperty]): JVMEnvDynFrame[NumProperty] =
    new JVMEnvDynFrame(domain, frame.clone, stack.clone, property widening that.propertyConformantWith(this))

  def tryCompareTo[B >: JVMEnvDynFrame[NumProperty]](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: JVMEnvDynFrame[NumProperty @unchecked] =>
      property.tryCompareTo(other.propertyConformantWith(this))
    case _ => None
  }

  /**
   * Convert a dynamic frame into a fixed frame
   */
  def toJVMEnvFixedFrame = {
    val bigProp = property.addVariables(frame.length + stack.length)
    val framedProp = (0 until frame.length).foldLeft(bigProp) {
      (prop, i) => if (frame(i) != -1) prop.variableAssignment(property.dimension + i, frame(i)) else prop
    }
    val stackedProp = (0 until stack.length).foldLeft(framedProp) {
      (prop, i) => if (stack(i) != -1) prop.variableAssignment(property.dimension + frame.length +  i, stack(i)) else prop
    }
    val finalProp = (0 until property.dimension).foldLeft(stackedProp) { (prop,_) => prop.delVariable(0)}
    val ffdom = new JVMEnvFixedFrameDomain(domain.dom)
    new JVMEnvFixedFrame(ffdom, frame.length, finalProp)
  }

  override def equals(that: Any) = that match {
    case that: JVMEnvDynFrame[NumProperty @unchecked] => property == that.property && frame == frame && stack == stack
    case _ => false
  }

  def mkString(vars: Seq[String]) =
    "Frame: " + frame.mkString("<", ",", ">") + " Stack: " + stack.mkString("<", ",", ">") + " Property: " + property

  override def toString =
    "Frame: " + frame.mkString("<", ",", ">") + " Stack: " + stack.mkString("<", ",", ">") + " Property: " + property

  def isTop = property.isTop

  def isBottom = isEmpty

  def isEmpty = property.isEmpty

  def top = new JVMEnvDynFrame(domain, frame, stack, property.top)

  def bottom = new JVMEnvDynFrame(domain, frame, stack, property.bottom)
}

/**
 * This is the abstract domain of JVM environments with dynamic frame. At the moment, it only deals with numerical
 * properties.
 * @param dom the numerical domain to use for the numerical variables.
 * @author Gianluca Amato <gamato@unich.it>
 */
class JVMEnvDynFrameDomain(val dom: NumericalDomain) extends JVMEnvDomain {
  type Property = JVMEnvDynFrame[dom.Property]

  def full(maxLocals: Int) = new JVMEnvDynFrame[dom.Property](this, Array.fill(maxLocals)(-1), Stack[Int](), dom.top(0))

  def empty(maxLocals: Int) = new JVMEnvDynFrame[dom.Property](this, Array.fill(maxLocals)(-1), Stack[Int](), dom.bottom(0))
}
