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
  * @author Gianluca Amato <amato@sci.unich.it>
  * @param low the lower bounds for the box
  * @param high the upper bounds for the box 
  */

final class BoxDouble(private val low: Array[Double], private val high: Array[Double]) extends NumericalDomain {   
  require(normalized,"The paramters low:"+ low.mkString(",") + " and high: "+ high.mkString(",") + " are not normalized")
  
  type Domain = BoxDouble

  /** 
   * This checks if the box is normalized. This should always be the case. A box is normalized when 
   * the lower and higher bounds are of the same length, and either a) there are no lower bounds equal to +Inf, 
   * there are no upper bounds equal to -Inf, the lower bounds are smaller of the corresponding upper bounds or
   * b) all the lower bound are +Inf and all the upper bounds are -Inf.
   * @return whether the box is normalized.
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
   * Returns the sum of x and y, rounded towards +Inf. 
   * @note This is not implemented correctly.
   * @param x first number to sum
   * @param y second number to som
   * @returns x+y rounded towards +Inf
   */
  private def add_hi(x: Double, y:Double): Double = x+y  
  
  /**
   * Returns the sum of x and y, rounded towards -Inf. 
   * @note This is not implemented correctly.
   * @param x first number to sum
   * @param y second number to som
   * @returns x+y rounded towards -Inf
   */
  private def add_lo(x: Double, y:Double): Double = x+y
  
  /**
   * Returns the product of x and y, rounded towards +Inf. Moreover, 
   * if x is 0, the product is 0 independently from the value of y.
   * @note This is not implemented correctly.
   * @param x first number to multiply
   * @param y second number to multiply
   * @returns x*y rounded towards +Inf
   */
  private def mul_hi(x: Double, y:Double): Double = if (x==0) 0 else x*y
  
  /**
   * Returns the product of x and y, rounded towards -Inf. Moreover, if x is 0, 
   * the product is 0 independently from the value of y.
   * @note This is not implemented correctly.
   * @param x first number to multiply
   * @param y second number to multiply
   * @returns x*y rounded towards -Inf
   */
  private def mul_lo(x: Double, y:Double): Double = if (x==0) 0 else x*y
  
  /**
   * Return the dot product of x and y, rounded towards -Inf.
   *   
   */
  private def dotprod_lo(x: Array[Double], y: Array[Double], remove : Int = -1): Double = {
    var sum : Double = 0
    for (i <- x.indices) if (i!=remove) sum = add_lo(sum,mul_lo(x(i),y(i)))
    return sum;
  }
  
  /**
   * Return the dot product of x and y, rounded towards +Inf.
   *   
   */
  private def dotprod_hi(x: Array[Double], y: Array[Double], remove : Int = -1): Double = {
    var sum : Double = 0
    for (i <- x.indices) if (i!=remove) sum =  add_hi(sum,mul_hi(x(i),y(i)))
    return sum;
  }

  
  /**
   * Computes the union of two boxes.
   * @param the box to be unioned with this
   * @return the union of the two boxes 
   */
  override def union(that: NumericalDomain): BoxDouble = {
    that match {
      case that: BoxDouble =>
      	require (dimension == that.dimension)
    	val newlow = (this.low, that.low).zipped.map(_ min _)
    	val newhigh = (this.high, that.high).zipped.map(_ max _)
    	new BoxDouble(newlow, newhigh)
     case _ => 
        throw new IllegalArgumentException      
    }
  }     
  
  override def widening(that: NumericalDomain): BoxDouble = {
    that match {
      case that: BoxDouble =>
        require (dimension == that.dimension)
        val newlow = (this.low, that.low).zipped.map ( (l1,l2) => if (l1== Double.PositiveInfinity) l2 else if (l1<=l2) l1 else Double.NegativeInfinity )
        val newhigh = (this.high, that.high).zipped.map ( (l1,l2) => if (l1== Double.NegativeInfinity) l2 else if (l1>=l2) l1 else Double.PositiveInfinity )
        new BoxDouble(newlow, newhigh)
      case _ => 
        throw new IllegalArgumentException    
    } 
  }
  
   override def narrowing(that: NumericalDomain): BoxDouble = {
    that match {
      case that: BoxDouble =>
        require (dimension == that.dimension)
        val newlow = (this.low, that.low).zipped.map ( (l1,l2) => if (l1 == Double.NegativeInfinity) l2 else l1 min l2 )
        val newhigh = (this.high, that.high).zipped.map ( (l1,l2) => if (l1 == Double.PositiveInfinity) l2 else l1 max l2 )
        new BoxDouble(newlow, newhigh)
      case _ => 
        throw new IllegalArgumentException    
    } 
  }
  
  
  /**
   * Compute the intersection of two boxes.
   * @param the box to be intersected with this
   * @return the intersection of the two boxes   
   */
  override def intersection(that: NumericalDomain): BoxDouble = {
    that match {
      case that: BoxDouble =>
    	require (dimension == that.dimension)
    	val newlow = (this.low, that.low).zipped.map(_ max _)
    	val newhigh = (this.high, that.high).zipped.map(_ min _)
    	new BoxDouble(newlow, newhigh)  
      case _ => 
        throw new IllegalArgumentException
    }
  }
  
  /**
   * Compute the minimum and maximum value of a linear form in a box.
   * @note should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @param known the unhomogeneous coefficient
   * @return a tuple with two components: the first component is the least value, the second component is the greatest value
   * 		 of the linear form over the tuple. 
   */
  private def linearEvaluation(coeff: Array[Double], known: Double): Tuple2[Double,Double] = {
    require(coeff.length <= dimension)
  	var newlow: Double = known
    var newhigh: Double = known     
    for (i <- 0 to coeff.length-1) {
      if (coeff(i) < 0) { 
        newlow = add_lo(newlow, mul_lo(coeff(i), high(i)))
        newhigh = add_hi(newhigh, mul_hi(coeff(i), low(i)))
      } else {
        newlow = add_lo(newlow, mul_lo(coeff(i), low(i)))
        newhigh = add_hi(newhigh, mul_hi(coeff(i), high(i)))
      }
    }   
    (newlow,newhigh)
  }
  
  /**
   * Compute the corner of the box which minimizes a linear form. We do not need the inhomogenous coefficients since it is not
   * relevant for the computation.
   * @note should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @return the coordinates of the point which minimizes the linear form
   */
  private def linearArgmin(coeff: Array[Double]): Array[Double] = {
    require(coeff.length <= dimension)
    coeff.padTo(dimension,0)
    (coeff zipWithIndex) map { case (c,i) => if (c>0) low(i) else high(i) }
  }
  
  /**
   * Compute the corner of the box which maximizes a linear form. We do not need the inhomogenous coefficients since it is not
   * relevant for the computation.
   * @note should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @return the coordinates of the point which maximizes the linear form
   */
  private def linearArgmax(coeff: Array[Double]): Array[Double] = {
    require(coeff.length == dimension)
    (coeff zipWithIndex) map { case (c,i) => if (c<0) low(i) else high(i) }    
  }
  
  /**
   * Linear assignment over a box. 
   * @note should be generalized to linear forms over arbitrary types.
   * @param n the variable to be reassigned
   * @param coeff the homogeneous coefficients
   * @param known the inhomogeneous coefficient
   * @return the least box which contains the result of the linear assignment
   * @throws IllegalArgumentException if parameters are not correct
   */  
  override def linearAssignment(n: Int, coeff: Array[Double], known: Double): BoxDouble = {    
    require(n <= low.length && coeff.length <= dimension)
    if (isEmpty) return this
    val interval = linearEvaluation(coeff,known)
    new BoxDouble(low.updated(n,interval._1),high.updated(n,interval._2))        
  }  

  /**
   * Intersection with an half-plane. It should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @param known the inhomogeneous coefficient
   * @return the least box which contains the intersection wih the half-plane coeff*v+knwown <= 0
   * @throws IllegalArgumentException if parameters are not correct
   */
  override def linearInequality(coeff: Array[Double], known: Double) : BoxDouble = {    
    require(coeff.length == dimension)
    
    /* if the box is empty the result is empty */
    if (isEmpty) return this  
    
    /* check if result is empty */
    val lfArgmin = linearArgmin(coeff)
    val lfMin = linearEvaluation(coeff,known)._1
    if (lfMin > 0) return BoxDouble.empty(dimension)
    
    val newlow = low.clone
    val newhigh = high.clone
    
    val infinities = (0 to (dimension-1)) filter { i => lfArgmin(i).isInfinity &&  coeff(i)!=0 } 
    infinities.size match {
      case 0 =>  
        for (i <- 0 to (coeff.length-1)) {
         if (coeff(i) < 0) newlow(i) = low(i) max lfArgmin(i)-lfMin / coeff(i)
         if (coeff(i) > 0) newhigh(i)= high(i) min  lfArgmin(i)-lfMin / coeff(i)
      }
      case 1 => {
        val posinf = infinities.head        
        if (coeff(posinf)<0) 
          newlow(posinf) = low(posinf) max  (dotprod_lo(coeff, lfArgmin, posinf)/lfArgmin(posinf))
        else
          newhigh(posinf) = high(posinf) min (dotprod_hi(coeff, lfArgmin, posinf)/lfArgmin(posinf))
      }
      case _ =>
    }   
    new BoxDouble(newlow, newhigh)
  }  
   
  /**
   * Intersection with the complements of a line. It should be generalized to linear forms over arbitrary types.
   * @param coeff the homogeneous coefficients
   * @param known the inhomogeneous coefficient
   * @return the least box which contains the intersection with the complement of the line coeff*v+knwown==0
   * @throws IllegalArgumentException if parameters are not correct
   */
  override def linearDisequality(coeff: Array[Double], known: Double) : BoxDouble = 
    throw new IllegalAccessException("Unimplemented feature");
  
  /*
  override def linearStrictInequality(n: Int, coeff: Array[Double], known: Double)
  override def linearEquality(n: Int, coeff: Array[Double], known: Double)
  */
    
  /**
   * Returns the dimension of the environment space.
   * @return the dimension of the environment space.
   */
  override val dimension: Int = low.length
  
  /** 
   * Test of emptyness
   * @return whether the abstract objecy is empty.
   */ 
  override def isEmpty: Boolean = (low, high).zipped.exists(_ > _)
  
  /**
   * Test for fullness
   * @return whether the abstract object represents the full environment space.
   */ 
  override def isFull: Boolean = low.forall(_ isNegInfinity) && high.forall(_ isPosInfinity)
    
        
  def tryCompareTo [B >: NumericalDomain](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
      case other: Domain =>  
        if (this.equals(other))
          Some(0)
        else if ((this.low, other.low).zipped.forall(_ <= _) &&  (this.high, other.high).zipped.forall(_ >= _))
          Some(1)
        else if ((this.low, other.low).zipped.forall(_ >=_) &&  (this.high, other.high).zipped.forall(_ <= _) )
          Some(-1)
        else
          None
      case _ => None
  }
  
  override def equals(other: Any): Boolean = other match {
    case other: Domain => java.util.Arrays.equals(this.low, other.low) && java.util.Arrays.equals(this.high, other.high)
    case _ => false
  }

  override def hashCode: Int = 41 * (41 + low.hashCode) + high.hashCode
  
  override def toString: String = 
    if (isEmpty)
      "[void]"
    else
      "[ <" + low.mkString(",") + "> --- <" + high.mkString(",") + "> ]"   
}

/**
 * This object provides a set of operations needed to work with boxes on doubles.
 */
object BoxDouble extends NumericalDomainFactory {  
  /**
   * This is a cache for empty boxes. In this way, we avoid to generate several different
   * objects for empty boxes.
   */
  private var cacheEmpty: Map[Int,BoxDouble] = Map()
  
  /** 
   * Returns a box with given bounds. The box is normalized: if it is empty, it is replaced by a well-behaved empty box
   * generated by the method empty.
   * @param low lower bounds
   * @param high upper bounds
   * @return the normalized box with the specified bounds
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
  
  def full(n: Int): BoxDouble = {
	new BoxDouble(Array.fill(n)(Double.NegativeInfinity), Array.fill(n)(Double.PositiveInfinity))
  }
  
  def empty(n: Int): BoxDouble = {
    if (! (cacheEmpty isDefinedAt n))  
      cacheEmpty += (n -> new BoxDouble(Array.fill(n)(Double.PositiveInfinity), Array.fill(n)(Double.NegativeInfinity)))
    cacheEmpty(n)        
  }

  private def unnormalizedIsEmpty(low: Array[Double], high: Array[Double]) = (low, high).zipped.exists(_ > _)

}
