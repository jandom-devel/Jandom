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

import it.unich.sci.jandom.domains.{AbstractDomain, NumericalDomain, NumericalProperty}

/**
 * This is the abstract property abstracting the environment (stack and frame) of a Java Virtual Machine. 
 * At the moment it only handles numerical variables. The class JVMEnv is mutable. Most operations are
 * the counterpart of bytecode operations.
 * @tparam Property the numerical property used to describe numerical variables
 * @param frame associates each element on the JVM frame to a dimension of `property`. The value `-1`
 * corresponds to a non-numerical local variable.
 * @param stack associates each element on the JVM stack to a dimension of `property`.  The value `-1`
 * corresponds to a non-numerical stack position.
 * @param property the numerical property describing the state of numerical variables.
 * @author Gianluca Amato <gamato@unich.it>
 */
class JVMEnv[Property <: NumericalProperty[Property]] (
  val frame: Array[Int], val stack: ArrayStack[Int], var property: Property) extends Cloneable {

  /**
   * Returns a deep copy of JVMEnv.
   */
  override def clone: JVMEnv[Property] =
    new JVMEnv(frame.clone, stack.clone, property)
  
  /**
   * Empties the abstract environment (i.e., it returns an abstract environment
   * representing no concrete environments).
   */
  def empty  {
    property = property.empty
  }
  
  def ipush(v: Int) {
    property = property.addDimension
    property = property.constantAssignment(property.dimension - 1, v)
    stack.push(property.dimension - 1)
  }

  def istore(i: Int) {
    val oldn = frame(i)
    val newn = stack.pop
    if (oldn != -1) property=property.delDimension(oldn)
    frame(i) =  if (oldn >= 0 && oldn < newn) newn-1 else newn
  }

  def iload(i: Int) {
    property = property.addDimension
    property = property.variableAssignment(property.dimension - 1, i)
    stack.push(property.dimension - 1)
  }

  def iadd() {
    val vm = stack.pop
    val vn = stack.top
    property = property.addAssignment(vn, vm).delDimension(vm)    
  }
  
  def if_icmpgt {
    val vm = stack.pop
    val vn = stack.pop
    property = property.if_icmpgt(vn, vm)
    property = property.delDimension(vm max vn)
    property = property.delDimension(vm min vn)
  }
  
  def if_icmple {
    val vm = stack.pop
    val vn = stack.pop
    property = property.if_icmple(vn, vm)
    property = property.delDimension(vm max vn)
    property = property.delDimension(vm min vn)
  }
  
  /**
   * Union of two abstract environments.
   * @param that the abstract environment to join with `this`
   * @return true if the result is bigger than `this`
   */
  def union(that: JVMEnv[Property]): Boolean = {
    // this should always hold!!
    //require(frame == that.frame)
    //require(stack == that.stack)
    val oldproperty = property
    property = property union that.property
    if (property > oldproperty)
      true
    else 
      false
  }

  /**
   * Widening of two abstract environments.
   * @param that the abstract environment to widen with `this`
   * @return true if the result is bigger than `this`
   */
  def widening(that: JVMEnv[Property]): Boolean = {
    // this should always hold!!
    //require(frame == that.frame)
    //require(stack == that.stack)
    val oldproperty = property
    property = property widening that.property
    if (property > oldproperty)
      true
    else 
      false
  }

  override def toString =
    "Frame: " + frame.mkString("<", ",", ">") + " Stack: " + stack.mkString("<", ",", ">") + " Property: " + property
}

/**
 * This is the abstract domain of JVM environments. At the moment, it only deals with numerical
 * variables.
 * @param dom the numerical domain to use for the numerical variables.
 * @author Gianluca Amato <gamato@unich.it>
 */
 class JVMEnvDomain(val dom: NumericalDomain) extends AbstractDomain {
   type Property = JVMEnv[dom.Property]
   
   /**
    * Creates a full JVM environment of dimension 0.
    * @param maxLocal maximum number of locals in the frame.
    */
   def full(maxLocals: Int) = new JVMEnv[dom.Property](Array.fill(maxLocals)(-1), ArrayStack[Int](), dom.full(0))

   /**
    * Creates an empty JVM environment of dimension 0.
    * @param maxLocal maximum number of locals in the frame.
    */
   
   def empty(maxLocals: Int) = new JVMEnv[dom.Property](Array.fill(maxLocals)(-1), ArrayStack[Int](), dom.empty(0))
   
}
