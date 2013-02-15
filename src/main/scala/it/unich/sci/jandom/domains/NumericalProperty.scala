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

package it.unich.sci.jandom
package domains

import annotations.AnnotationType

/**
 * Base trait for numerical property operations. A concrete class `C` implementing a numerical
 * property should mix-in `NumericalProperty[C]`. Note that binary operations only works between
 * compatible properties, i.e. properties over vector spaces of the same dimension.
 * 
 * @tparam Property the property type we attach to and provide numerical operations.
 * @author Gianluca Amato <amato@sci.unich.it>
 * @define PPL [[http://bugseng.com/products/ppl/ PPL]] 
 * @define APRON [[http://apron.cri.ensmp.fr/library/ APRON]]
 */
trait NumericalProperty[Property] extends AbstractProperty with PartiallyOrdered[Property] {
    
  /**
   * The standard widening for two abstract objects. 
   * @param that the abstract object to be widened with this. `That` is NOT assumed to be bigger than `this`. 
   * @return the widening of the two abstract objects.
   */  
  def widening(that: Property): Property 
  
  /**
   * The standard widening for two abstract objects.
   * @param that the abstract object to be narrowed with this. `That` IS assumed to be smaller than `this`.
   * @note `That` should be be bigger than `this`. 
   * @return the narrowing of the two abstract objects.
   */  
  def narrowing(that: Property): Property
  
  /**
   * Union of two abstract objects.
   * @param that the abstract object to be joined with this.
   * @return the union of the two abstract objects.
   */
  def union(that: Property): Property
  
  /**
   * Intersection of two abstract objects.
   * @param that the abstract object to be intersected with this.
   * @return the intersection of the two abstract objects.
   */
  def intersection(that: Property): Property
  
  /**
   * Non deterministic assignment (also called `forget` operator)
   * @param n the variable to which non-deterministic assignment should be applied.  
   */
  def nonDeterministicAssignment(n: Int): Property 
  
  /**
   * Linear assignment over an abstract object. It should be generalized to linear forms over arbitrary types.
   * @param n the variable to be reassigned.
   * @param coeff the homogeneous coefficients.
   * @param known the in-homogeneous coefficient.
   */
  def linearAssignment(n: Int, coeff:Array[Double], known: Double): Property
  
   /**
   * Intersection with an half-plane. It should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients.
   * @param known the in-homogeneous coefficient.
   */
  def linearInequality(coeff: Array[Double], known: Double): Property
  
   /**
   * Intersection with the complements of a line. It should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients.
   * @param known the in-homogeneous coefficient.
   */
  def linearDisequality(coeff: Array[Double], known: Double): Property
  
  /**
   * Returns the dimension of the environment space.
   * @return the dimension of the environment space.
   */
  def dimension: Int
  
  /** 
   * Test of emptiness
   * @return whether the abstract object is empty.
   */
  def isEmpty: Boolean
  
  /**
   * Test for fullness.
   * @return whether the abstract object represents the full environment space.
   */
  def isFull: Boolean    
  
  /**
   * Returns an empty object compatible with `this`.
   */
  def empty: Property
  
  /**
   * Returns a full object compatible with `this`.
   */
  def full: Property

  /** 
   * Returns the string representation of the property. It calls `mkString` with the standard
   * variables names `v1` ... `vn`. 
   */
  override def toString: String = "[ " + (mkString( for (i<- 0 until dimension) yield "v"+i )).mkString(" , ") + " ]"
}
