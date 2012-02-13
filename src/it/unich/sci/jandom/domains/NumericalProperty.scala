/**
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
 *
 * (c) 2011,2012 Gianluca Amato
 */

package it.unich.sci.jandom.domains

/**
 * Trait for numerical properties, such as Box, Octagon, etc... The classes extending NumericalProperty should be 
 * immutables. 
 * 
 * @author Gianluca Amato <amato@sci.unich.it>
 */
trait NumericalProperty[Property] extends PartiallyOrdered[Property] {
  
  /**
   * Widening of two abstract objects.
   * @param that the abstract object to be widened with this
   * @return the widening of the two abstract objects
   */
  def widening(that: Property): Property
  
  /**
   * Narrowing of two abstract objects.
   * @param that the abstract object to be narrowed with this
   * @return the narrowing of the two abstract objects
   */
  def narrowing(that: Property): Property
  
  /**
   * Union of two abstract objects.
   * @param that the abstract object to be unioned with this
   * @return the union of the two abstract objects
   */
  def union(that: Property): Property
  
  /**
   * Intersection of two abstract objects.
   * @param that the abstract object to be intersected with this.
   * @return the intersection of the two abstract objects
   */
  def intersection(that: Property): Property
  
  /**
   * Linear assignment over an abstract object. It should be generalized to linear forms over arbitrary types.
   * @param n the variable to be reassigned
   * @param coeff the homogeneous coefficients
   * @param known the unhomogeneous coefficient
   */
  def linearAssignment(n: Int, coeff:Array[Double], known: Double): Property
  
   /**
   * Intersection with an half-plane. It should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @param known the inhomogeneous coefficient
   */
  def linearInequality(coeff: Array[Double], known: Double): Property
  
   /**
   * Intersection with the complements of a line. It should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @param known the inhomogeneous coefficient
   */
  def linearDisequality(coeff: Array[Double], known: Double): Property
  
  /**
   * Returns the dimension of the environment space.
   * @return the dimension of the environment space.
   */
  def dimension: Int
  
  /** 
   * Test of emptyness
   * @return whether the abstract objecy is empty.
   */
  def isEmpty: Boolean
  
  /**
   * Test for fullness
   * @return whether the abstract object represents the full environment space.
   */
  def isFull: Boolean    
}

/** 
 * Trait for numerical domains. A numerical domains should produce numerical properties.
 */
trait NumericalDomain[Property] {
  /**
   * Create an abstract object representing the full n-dimensional space.
   * @param n the dimension of the environment space.
   * @return the full n-dimensional environment space.
   */
  def full(n:Int): Property
  
  /**
   * Create an abstract object representing the empty n-dimensional space.
   * @param n the dimension of the environment space.
   * @return the empty n-dimensional environment space.
   */
  def empty(n:Int): Property
}
