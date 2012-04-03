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
 * (c) 2012 Gianluca Amato
 */

package it.unich.sci.jandom.domains

import parma_polyhedra_library.C_Polyhedron
import parma_polyhedra_library.Linear_Expression
import parma_polyhedra_library.Linear_Expression_Coefficient
import parma_polyhedra_library.Linear_Expression_Variable
import parma_polyhedra_library.Variable
import parma_polyhedra_library.Coefficient
import parma_polyhedra_library.Relation_Symbol
import parma_polyhedra_library.Constraint
import parma_polyhedra_library.Degenerate_Element
import it.unich.sci.jandom.widenings.Widening

/**
 * The class for Polyhedra in PPL.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class PPLCPolyhedron (private val pplpolyhedron : C_Polyhedron) extends NumericalProperty[PPLCPolyhedron] {
     
  def widening(that: PPLCPolyhedron): PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.upper_bound_assign(that.pplpolyhedron)
    newpplpolyhedron.widening_assign(pplpolyhedron,null)    
    new PPLCPolyhedron(newpplpolyhedron)
  }
  
  def narrowing(that: PPLCPolyhedron): PPLCPolyhedron = {
    this
  }   

  def union(that: PPLCPolyhedron): PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)    
    val x = new C_Polyhedron(pplpolyhedron.space_dimension(), Degenerate_Element.EMPTY)
    newpplpolyhedron.upper_bound_assign(that.pplpolyhedron)
    new PPLCPolyhedron(newpplpolyhedron)
  }
  
  def intersection(that: PPLCPolyhedron): PPLCPolyhedron = {
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.intersection_assign(that.pplpolyhedron)
    new PPLCPolyhedron(newpplpolyhedron)
  }

  def linearAssignment(n: Int, coeff: Array[Double], known: Double): PPLCPolyhedron = { 
    val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
    newpplpolyhedron.affine_image(new Variable(n), toPPLLinearExpression(coeff,known), new Coefficient(1))
    new PPLCPolyhedron(newpplpolyhedron)
  }

  def linearInequality(coeff: Array[Double], known: Double): PPLCPolyhedron = { 
	 val le = toPPLLinearExpression(coeff,known)
	 val newpplpolyhedron = new C_Polyhedron(pplpolyhedron)
	 newpplpolyhedron.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
	 new PPLCPolyhedron(newpplpolyhedron)
  }

  def linearDisequality(coeff: Array[Double], known: Double): PPLCPolyhedron = {
     throw new IllegalAccessException("Unimplemented feature");
  }

  def dimension(): Int = pplpolyhedron.space_dimension.toInt

  def isEmpty(): Boolean = pplpolyhedron.is_empty

  def isFull(): Boolean = pplpolyhedron.is_universe
  
  def empty = PPLCPolyhedron.empty(pplpolyhedron.space_dimension.toInt)
  
  def full = PPLCPolyhedron.full(pplpolyhedron.space_dimension.toInt)

  def tryCompareTo [B >: PPLCPolyhedron](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: PPLCPolyhedron => 
      if (pplpolyhedron==other.pplpolyhedron) Some(0)  else 
        if (pplpolyhedron strictly_contains other.pplpolyhedron) Some(1) else 
          if (other.pplpolyhedron strictly_contains pplpolyhedron) Some(-1)
          	else None
    case _ => None
  }
  
  override def equals(other: Any): Boolean = other match {
    case other: PPLCPolyhedron => pplpolyhedron.equals(other.pplpolyhedron)
    case _ => false
  }

  override def hashCode: Int = pplpolyhedron.hashCode
  
  override def toString : String = pplpolyhedron.toString
  
  private def toPPLLinearExpression(coeff:Array[Double], known:Double): Linear_Expression = {
    var le : Linear_Expression = new Linear_Expression_Coefficient(new Coefficient(known.toInt))
	for (i <- 0 to (coeff.length - 1)) {
	  le = le.sum ( (new Linear_Expression_Variable(new Variable(i)).times(new Coefficient(coeff(i).toInt)) ))
	}
    return le
  }
}

/**
 * This is the PPLCPolyhedron domain.
 */
object PPLCPolyhedron extends NumericalDomain[PPLCPolyhedron] {  

  def full(n: Int): PPLCPolyhedron = {
    val pplpolyhedron = new C_Polyhedron(n, Degenerate_Element.UNIVERSE)
	new PPLCPolyhedron(pplpolyhedron)
  }
  
  def empty(n: Int): PPLCPolyhedron = {
    val pplpolyhedron = new C_Polyhedron(n, Degenerate_Element.EMPTY)
    new PPLCPolyhedron(pplpolyhedron)        
  }  
}
