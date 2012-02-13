/**
 * This file is part of JANDOM: JVM-based Analyzer for Numerical PPLBoxDoubles
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

import parma_polyhedra_library.Double_Box
import parma_polyhedra_library.Linear_Expression
import parma_polyhedra_library.Linear_Expression_Coefficient
import parma_polyhedra_library.Linear_Expression_Variable
import parma_polyhedra_library.Variable
import parma_polyhedra_library.Coefficient
import parma_polyhedra_library.Relation_Symbol
import parma_polyhedra_library.Constraint
import parma_polyhedra_library.Degenerate_Element
import com.sun.xml.internal.bind.v2.TODO

/**
 * The class for Boxes in PPL.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class PPLBoxDouble(private val pplbox : Double_Box) extends NumericalProperty[PPLBoxDouble] {
  
  def widening(that: PPLBoxDouble): PPLBoxDouble = { 
    val newpplbox = new Double_Box(pplbox)
    newpplbox.CC76_widening_assign(that.pplbox, null)
    new PPLBoxDouble(newpplbox)              
  }  

  def narrowing(that: PPLBoxDouble): PPLBoxDouble = {   
    val newpplbox = new Double_Box(pplbox)   
    newpplbox.CC76_narrowing_assign(that.pplbox)
    new PPLBoxDouble(newpplbox)                            
  }   

  def union(that: PPLBoxDouble): PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)    
    val x = new Double_Box(pplbox.space_dimension(), Degenerate_Element.EMPTY)
    newpplbox.upper_bound_assign(that.pplbox)
    new PPLBoxDouble(newpplbox)
  }
  
  def intersection(that: PPLBoxDouble): PPLBoxDouble = {
    val newpplbox = new Double_Box(pplbox)
    newpplbox.intersection_assign(that.pplbox)
    new PPLBoxDouble(newpplbox)
  }

  def linearAssignment(n: Int, coeff: Array[Double], known: Double): PPLBoxDouble = { 
    val newpplbox = new Double_Box(pplbox)
    newpplbox.affine_image(new Variable(n), toPPLLinearExpression(coeff,known), new Coefficient(1))
    new PPLBoxDouble(newpplbox)
  }

  def linearInequality(coeff: Array[Double], known: Double): PPLBoxDouble = { 
	 val le = toPPLLinearExpression(coeff,known)
	 val newpplbox = new Double_Box(pplbox)
	 newpplbox.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
	 new PPLBoxDouble(newpplbox)
  }

  def linearDisequality(coeff: Array[Double], known: Double): PPLBoxDouble = {
     throw new IllegalAccessException("Unimplemented feature");
  }

  def dimension(): Int = pplbox.space_dimension().toInt

  def isEmpty(): Boolean = pplbox.is_empty

  def isFull(): Boolean = pplbox.is_universe

  def tryCompareTo [B >: PPLBoxDouble](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: PPLBoxDouble => if (pplbox.equals(other.pplbox)) Some(0) else None        
    case _ => None
  }
  
  override def toString : String = pplbox.toString
  
  private def toPPLLinearExpression(coeff:Array[Double], known:Double): Linear_Expression = {
    var le : Linear_Expression = new Linear_Expression_Coefficient(new Coefficient(known.toInt))
	for (i <- 0 to (coeff.length - 1)) {
	  le = le.sum ( (new Linear_Expression_Variable(new Variable(i)).times(new Coefficient(coeff(i).toInt)) ))
	}
    return le
  }
}

/**
 * This is the PPLBoxDouble domain.
 */
object PPLBoxDouble extends NumericalDomain[PPLBoxDouble] {  

  def full(n: Int): PPLBoxDouble = {
    val pplbox = new Double_Box(n, Degenerate_Element.UNIVERSE)
	new PPLBoxDouble(pplbox)
  }
  
  def empty(n: Int): PPLBoxDouble = {
    val pplbox = new Double_Box(n, Degenerate_Element.EMPTY)
    new PPLBoxDouble(pplbox)        
  }  

}
