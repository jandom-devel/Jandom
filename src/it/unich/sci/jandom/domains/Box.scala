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
  * This is the box abstract domain over doubles.
  * 
  * It is actually a proof of concept on how to implements an abstract domain entirely in Scala. 
  * Real domains should be implemented within PPL or APRON. 
  *  
  * @author Gianluca Amato <amato@sci.unich.it> */
final class BoxDouble(private val low: Array[Double], private val high: Array[Double]) extends NumericalDomain {   
  require(normalized,"The paramters low:"+ low.mkString(",") + " and high: "+ high.mkString(",") + " are not normalized")
  
  type Domain = BoxDouble

  /** 
   * This checks if the box is normalized. This should always be the case. A box is normalized when 
   * the lower and higher bounds are of the same length, and either a) there are no lower bounds equal to +Inf, 
   * there are no upper bounds equal to -Inf, the lower bounds are smaller of the corresponding upper bounds or
   * b) all the lower bound are +Inf and all the upper bounds are -Inf
   */
  private def normalized : Boolean =  
    low.length == high.length && 
    ( 
        low.forall { (x) => ! (x isPosInfinity) } &&
        high.forall { (x) => ! (x isNegInfinity) } &&
        (low, high).zipped.forall(_ <= _)
    ||
    	low.forall { _ isPosInfinity } &&
    	high.forall { _ isNegInfinity }
    )
      
  /**
   * Returns the sum of x and y, rounded towards +Inf
   * 
   * @param x first number to sum
   * @param y second number to som
   * @returns x+y rounded towards +Inf
   */
  private def add_hi(x: Double, y:Double): Double = x+y  
  private def add_lo(x: Double, y:Double): Double = x+y
  
  /**
   * Returns the product of x and y, rounded towards +Inf. Moreover, if x is 0, 
   * the product is 0 independently from the value of y.
   * 
   * @param x first number to multiply
   * @param y second number to multiply
   * @returns x*y rounded towards +Inf
   */
  private def mul_hi(x: Double, y:Double): Double = if (x==0) 0 else x*y
  
  /**
   * Returns the product of x and y, rounded towards -Inf. Moreover, if x is 0, 
   * the product is 0 independently from the value of y.
   * 
   * @param x first number to multiply
   * @param y second number to multiply
   * @returns x*y rounded towards -Inf
   */
  private def mul_lo(x: Double, y:Double): Double = if (x==0) 0 else x*y
  
  /**
   * Returns the union of the box with another box.
   * @param that the other box
   * @return the union of the two boxes 
   */
  override def union(that: BoxDouble): BoxDouble = {
    require (dimension == that.dimension)
    val newlow = (this.low, that.low).zipped.map(_ min _)
    val newhigh = (this.high, that.high).zipped.map(_ max _)
    new BoxDouble(newlow, newhigh)
  }
  
  /**
   * Returns the intersection of the box with another box.
   * @param that the other box
   * @return the intersection of the two boxes 
   */
  override def intersection(that: BoxDouble): BoxDouble = {
    require (dimension == that.dimension)
    val newlow = (this.low, that.low).zipped.map(_ max _)
    val newhigh = (this.high, that.high).zipped.map(_ min _)
    new BoxDouble(newlow, newhigh)   
  }  
  
  /**
   * Returns the new box given as a result of a linear assignment.
   * 
   * @param coeff the array of coefficients for the linear assignment. The last element
   * is the constant term.
   * @param n the index of the variable we want to update with the assignment
   * @return the new box
   * @throws IllegalArgumentException if parameters are not correct
   */
  override def linearAssignment(n: Int, coeff: Array[Double], known: Double): BoxDouble = {
    require(n <= low.length && coeff.length == dimension)
    if (isEmpty) return this
    var newlow: Double = known
    var newhigh: Double = known 
    for (i <- (0 to dimension-1)) {
      if (coeff(i) < 0) { 
        newlow = add_lo(newlow, mul_lo(coeff(i), high(i)))
        newhigh = add_hi(newhigh, mul_hi(coeff(i), low(i)))
      } else {
        newlow = add_lo(newlow, mul_lo(coeff(i), low(i)))
        newhigh = add_hi(newhigh, mul_hi(coeff(i), high(i)))
      }
    }    
    new BoxDouble(low.updated(n,newlow),high.updated(n,newhigh))        
  }
  
  
  override def linearInequality(coeff: Array[Double], known: Double) : BoxDouble = {
    
    this
  }  
  /*
  override def linearDisquality(n: Int, coeff: Array[Double], known: Double)
  override def linearStrictInequality(n: Int, coeff: Array[Double], known: Double)
  override def linearEquality(n: Int, coeff: Array[Double], known: Double)
  */
  
  /**
   * Returns the dimension of the environment space.
   * 
   * @return the dimension of the environment space.
   */
  override val dimension: Int = low.length
  
  /**
   * Returns true if the box is empty
   * 
   * @return true if the box is empty
   */
  override def isEmpty: Boolean = (low, high).zipped.exists(_ > _)
  /**
   * Returns true if the box contains the full environment space
   * 
   * @return true if the box contains the full environment space
   */
  override def isFull: Boolean = low.forall(_ isNegInfinity) && high.forall(_ isPosInfinity)
    
  override def equals(other: Any): Boolean = other match {
    case other: Domain => java.util.Arrays.equals(this.low, other.low) && java.util.Arrays.equals(this.high, other.high)
    case _ => false
  }

  override def hashCode: Int = 41 * (41 + low.hashCode) + high.hashCode
  
  override def toString: String = 
    if (isEmpty)
      "void"
    else
      "[ <" + low.mkString(",") + "> --- <" + high.mkString(",") + "> ]"
}

/**
 * This object provides a set of operations needed to work with boxes on doubles
 */
object BoxDouble {  
  /**
   * This is a cache for empty boxes. In this way, we avoid to generate several different
   * objects for empty boxes.
   */
  private var cacheEmpty: Map[Int,BoxDouble] = Map()
  
  /** Returns a box whose lowerbounds/upperbounds are given by low and upper repsectively
   * 
   * The box is normalized: if it is empty, it is replaced by a well-behaved empty box
   * generated by the method empty.
   * 
   * @param low lower bounds
   * @param high upper bounds
   * @return the normalized box with the specified values
   * @throws IllegalArgumentExpection if the length of low and high is not the same
   */
  def apply(low: Array[Double], high: Array[Double]): BoxDouble = {
    require(low.length==high.length)
    if (unnormalizedIsEmpty(low,high)) 
      empty(low.length)
    else
      new BoxDouble(low, high)
  }
    
  def apply(poDouble: Array[Double]): BoxDouble = apply(poDouble, poDouble)
  
  def empty(n: Int): BoxDouble = {
    if (! (cacheEmpty isDefinedAt n))  
      cacheEmpty += (n -> new BoxDouble(Array.fill(n)(Double.PositiveInfinity), Array.fill(n)(Double.NegativeInfinity)))
    cacheEmpty(n)        
  }

  private def unnormalizedIsEmpty(low: Array[Double], high: Array[Double]) = (low, high).zipped.exists(_ > _)

}
