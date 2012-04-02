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
package it.unich.sci.jandom.targets

/**
 * The class LinearForm represents a homogeneous linear form over a numeric type. Variables are not
 * explicitly handled, they are just positions in the array of coefficients. 
 * @param coefficients the coefficients of the linear form
 * @author Gianluca Amato <amato@sci.unich.it>
 */

class LinearForm[T](val coefficients: Seq[T], val env: Environment) (implicit numeric: Numeric[T]) {
  
  import numeric._
     
  /**
   * Equality between linear forms. Two linear forms are equal if their coefficients are the same and
   * are defined over the same environment
   */
  override def equals(other: Any): Boolean = other match {
    case other: LinearForm[T] => 
      (env == other.env) && (
          (coefficients zip other.coefficients) forall (tuple => tuple._1 == tuple._2)
       )    	     	 
    case _ => false
  }
  
  def unary_-(): LinearForm[T] = new LinearForm( coefficients map ( x => -x ), env ) 
  
  def +(other: LinearForm[T]): LinearForm[T] =  {
    require(env == other.env)
    new LinearForm ( coefficients.zipAll(other.coefficients,zero,zero) map ( pair => pair._1 + pair._2 ), env) 
  }     
  
  def -(other: LinearForm[T]): LinearForm[T] = this + (-other)
  
  def *(coeff: T): LinearForm[T]  =  new LinearForm( coefficients map ( _*coeff ), env )      
  
  /**
   * Returns the textual representation of a linear form
   */
  override def toString =  {    
    var first = true
    var index = 0
    var s = ""
        
    for ( coeff <- coefficients ) {      
      val term = coeff match {
        case 0 => ""
        case 1 => if (index == 0) "1" else env(index-1)
        case -1 => if (index == 0) "-1" else "-"+env(index-1)
        case c:T => c.toString + (if (index==0) "" else "*"+env(index-1))
      }
      if (coeff!=0) {
    	if (first || coeff < zero) {
          s += term 
          first = false
        } else if (coeff!=0) 
          s += "+"+term        
      }
      index += 1
    }
    if (s.isEmpty) "0" else s      
  }
  
  /**
   * Return the constant (inhomogeneous) term of the linear form
   */
  def known: T = coefficients.head
  
  /**
   * Return the homonogeneous terms in the linear form
   */
  def homcoeff: Seq[T] = coefficients.tail
}
  
object LinearForm {
  def apply[T](coeffs: Seq[T], env: Environment)(implicit numeric: Numeric[T]) = new LinearForm(coeffs, env)    
  def fromCoefficient[T](coeff: T, env: Environment)(implicit numeric: Numeric[T]) = fromCoefficientVar(coeff,0,env)
  def fromVar[T](v: Int, env: Environment)(implicit numeric: Numeric[T]) = fromCoefficientVar(1,v,env)
  def fromCoefficientVar[T](coeff:T, v: Int, env: Environment)(implicit numeric: Numeric[T]) = new LinearForm ( List.fill(v)(numeric.zero) ++ List(coeff), env )   
}

