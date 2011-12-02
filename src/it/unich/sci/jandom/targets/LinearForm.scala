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
 * The class LinearForm represents a linear form over a Field. The Field is a
 * type argument of the class and should be specified in all its istances.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

import scala.collection.mutable._

class LinearForm[T](val coefficients: List[T]) (implicit numeric: Numeric[T]) {
  
  import numeric._
  
  def unary_-(): LinearForm[T] = 	
	new LinearForm( coefficients map ( x => -x ) ) 
  
  def +(other: LinearForm[T]): LinearForm[T] = {  
    val newcoeff = coefficients.zip(other.coefficients) map ( pair => pair._1 + pair._2 ) 
    new LinearForm(newcoeff)    
  }
  
  def -(other: LinearForm[T]): LinearForm[T] = this + (-other)
  
  def *(coeff: T): LinearForm[T]  = {	
	new LinearForm( coefficients map (  x=> x*coeff) )
  }

  override def equals(other: Any): Boolean = other match {
    case other:LinearForm[T] => coefficients == other.coefficients
    case _ => false
  } 
        
  override def toString =  {
    var first = true
    var index = 0
    var s = ""
        
    while ( index < coefficients.size ) {
      val coeff = coefficients(index)
      val term = coeff match {
        case 0 => ""
        case 1 => if (index == 0) "1" else "v"+index
        case -1 => if (index == 0) "1" else "-v"+index
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
}
  
object LinearForm {  
  def apply[T](n:Int)(implicit numeric: Numeric[T]) = new LinearForm(List.fill(n)(numeric.zero))
  def fromVar[T](v: Int)(implicit numeric: Numeric[T]) = new LinearForm((List.fill(v-1)(numeric.zero)) ++ List(numeric.one))
  def fromCoefficient[T](coeff: T)(implicit numeric: Numeric[T]) = new LinearForm( List(coeff) )
  def fromCoefficientVar[T](coeff:T, v: Int)(implicit numeric: Numeric[T]) = new LinearForm((List.fill(v-1)(numeric.zero)) ++ List(coeff))
}

