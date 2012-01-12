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
 * The class LinearForm represents a non-homogeneous linear form over a numeric type. 
 * @param coefficients the coefficients of the linear form
 * @author Gianluca Amato <amato@sci.unich.it>
 */

class LinearForm[T](val coefficients: Seq[T], val environment: Environment) (implicit numeric: Numeric[T]) {
  
  import numeric._
     
  override def equals(other: Any): Boolean = other match {
    case other: LinearForm[T] => 
    	(coefficients zip other.coefficients) forall (tuple => tuple._1 == tuple._2)     	  
    case _ => false
  }
  
  def unary_-(): LinearForm[T] = 	
	new LinearForm( coefficients map ( x => -x ), environment: Environment ) 
  
  def +(other: LinearForm[T]): LinearForm[T] = {  
    val newcoeff = coefficients.zipAll(other.coefficients,zero,zero) map ( pair => pair._1 + pair._2 ) 
    new LinearForm(newcoeff, environment: Environment)    
  }
  
  def -(other: LinearForm[T]): LinearForm[T] = this + (-other)
  
  def *(coeff: T): LinearForm[T]  = {	
	new LinearForm( coefficients map ( _*coeff ),  environment: Environment )
  }       
  
  override def toString =  {
    var first = true
    var index = 0
    var s = ""
    val names = environment.getVariableNames.toArray
        
    for ( coeff <- coefficients ) {      
      val term = coeff match {
        case 0 => ""
        case 1 => if (index == 0) "1" else names(index-1)
        case -1 => if (index == 0) "1" else "-"+names(index-1)
        case c:T => c.toString + (if (index==0) "" else "*v" + index)
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
  
  def homcoeff = coefficients.tail.map { x => x.toDouble() }.padTo(environment.getNumVariables,0.0).toArray
  
  def known = coefficients.head.toDouble
}
  
object LinearForm {
  def apply[T](coeffs: Seq[T])(implicit numeric: Numeric[T])  = {
    val env = new Environment()
    (1 to coeffs.length) foreach { index => env.addVariable( "v"+ index ) }
    new LinearForm(coeffs, env)
  }
  def fromCoefficient[T](coeff: T, environment: Environment = null)(implicit numeric: Numeric[T]) = fromCoefficientVar(coeff,0, environment: Environment)
  def fromVar[T](v: Int, environment: Environment = null )(implicit numeric: Numeric[T]) = fromCoefficientVar(1,v, environment: Environment)
  def fromCoefficientVar[T](coeff:T, v: Int, env: Environment = null )(implicit numeric: Numeric[T]) = {
    val coeffs = List.fill(v)(numeric.zero) ++ List(coeff)
    if (env == null)
      apply(coeffs)(numeric)
    else 
      new LinearForm(coeffs, env)
  } 
}

