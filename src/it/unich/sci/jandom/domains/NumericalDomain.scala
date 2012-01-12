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
 * (c) 2011 Gianluca Amato
 */

package it.unich.sci.jandom.domains

/**
 * Trait for numerical domains, such as Box, Octagon, etc... The classes extending NumericalDomain should be 
 * immutables. 
 * 
 * @author Gianluca Amato <amato@sci.unich.it>
 */
trait NumericalDomain extends PartiallyOrdered[NumericalDomain] {
  type Domain <: NumericalDomain 
  
  
  /**
   * Widening of two abstract objects.
   * @param that the abstract object to be widened with this
   * @return the widening of the two abstract objects
   */
  def widening(that: NumericalDomain): Domain
  
  /**
   * Narrowinf of two abstract objects.
   * @param that the abstract object to be narrowed with this
   * @return the narrowing of the two abstract objects
   */
  def narrowing(that: NumericalDomain): Domain
  
  /**
   * Union of two abstract objects.
   * @param that the abstract object to be unioned with this
   * @return the union of the two abstract objects
   */
  def union(that: NumericalDomain): Domain
  
  /**
   * Intersection of two abstract objects.
   * @param that the abstract object to be intersected with this.
   * @return the intersection of the two abstract objects
   */
  def intersection(that: NumericalDomain): Domain  
  
  /**
   * Linear assignment over an abstract object. It should be generalized to linear forms over arbitrary types.
   * @param n the variable to be reassigned
   * @param coeff the homogeneous coefficients
   * @param known the unhomogeneous coefficient
   */
  def linearAssignment(n: Int, coeff:Array[Double], known: Double): Domain
  
   /**
   * Intersection with an half-plane. It should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @param known the inhomogeneous coefficient
   */
  def linearInequality(coeff:Array[Double], known: Double): Domain
  
   /**
   * Intersection with the complements of a line. It should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @param known the inhomogeneous coefficient
   */
  def linearDisequality(coeff: Array[Double], known: Double): Domain
  
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
 * Trait for factories of numerical domains. A factory should produce abstract objects.
 */
trait NumericalDomainFactory {
  /**
   * Create an abstract object representing the full n-dimensional space.
   * @param n the dimension of the environment space.
   * @return the full n-dimensional environment space.
   */
  def full(n:Int): NumericalDomain
  
  /**
   * Create an abstract object representing the empty n-dimensional space.
   * @param n the dimension of the environment space.
   * @return the empty n-dimensional environment space.
   */
  def empty(n:Int): NumericalDomain
}
