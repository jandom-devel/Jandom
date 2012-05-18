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

package it.unich.sci.jandom
package domains

import parma_polyhedra_library.C_Polyhedron
import parma_polyhedra_library.Linear_Expression
import parma_polyhedra_library.Linear_Expression_Coefficient
import parma_polyhedra_library.Linear_Expression_Variable
import parma_polyhedra_library.Variable
import parma_polyhedra_library.Coefficient
import parma_polyhedra_library.Relation_Symbol
import parma_polyhedra_library.Constraint
import parma_polyhedra_library.Degenerate_Element

/**
 * The domain for not necessarily closed polyhedra implemented within $PPL. This is essentially
 * a wrapper transforming methods of `C_Polyhedron` to methods of `NumericalProperty`. We clone
 * objects in order have an immutable class.
 * @param pplpolyhedron an object of class `C_Polyhedron` which is the $PPL wrapped object.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class PPLCPolyhedronRefl(val pplobject: C_Polyhedron) extends PPLNumericalProperty[PPLCPolyhedronRefl] {
  
  override def widening(that: PPLCPolyhedronRefl): PPLCPolyhedronRefl = {
    val newpplpolyhedron = new C_Polyhedron(pplobject)
    newpplpolyhedron.upper_bound_assign(that.pplobject)
    newpplpolyhedron.widening_assign(pplobject,null)    
    new PPLCPolyhedronRefl(newpplpolyhedron)
  }
  
  /**
   * Since there is no standard narrowing for polyehdra, this is a fake narrowing which
   * always return `this`.
   */
  override def narrowing(that: PPLCPolyhedronRefl): PPLCPolyhedronRefl = {
    this
  }   

  def union(that: PPLCPolyhedronRefl): PPLCPolyhedronRefl = {
    val newpplpolyhedron = new C_Polyhedron(pplobject)    
    val x = new C_Polyhedron(pplobject.space_dimension(), Degenerate_Element.EMPTY)
    newpplpolyhedron.upper_bound_assign(that.pplobject)
    new PPLCPolyhedronRefl(newpplpolyhedron)
  }
  
  def intersection(that: PPLCPolyhedronRefl): PPLCPolyhedronRefl = {
    val newpplpolyhedron = new C_Polyhedron(pplobject)
    newpplpolyhedron.intersection_assign(that.pplobject)
    new PPLCPolyhedronRefl(newpplpolyhedron)
  }

  def linearAssignment(n: Int, coeff: Array[Double], known: Double): PPLCPolyhedronRefl = { 
    val newpplpolyhedron = new C_Polyhedron(pplobject)
    newpplpolyhedron.affine_image(new Variable(n), toPPLLinearExpression(coeff,known), new Coefficient(1))
    new PPLCPolyhedronRefl(newpplpolyhedron)
  }

  def linearInequality(coeff: Array[Double], known: Double): PPLCPolyhedronRefl = { 
	 val le = toPPLLinearExpression(coeff,known)
	 val newpplpolyhedron = new C_Polyhedron(pplobject)
	 newpplpolyhedron.refine_with_constraint(new Constraint(le, Relation_Symbol.LESS_OR_EQUAL, new Linear_Expression_Coefficient(new Coefficient(0))))
	 new PPLCPolyhedronRefl(newpplpolyhedron)
  }

  def linearDisequality(coeff: Array[Double], known: Double): PPLCPolyhedronRefl = {
     throw new IllegalAccessException("Unimplemented feature");
  }
  
  def empty() = PPLCPolyhedronRefl.empty(pplobject.space_dimension.toInt)
  
  def full() = PPLCPolyhedronRefl.full(pplobject.space_dimension.toInt)

  def tryCompareTo [B >: PPLCPolyhedronRefl](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = other match {
    case other: PPLCPolyhedronRefl => 
      if (pplobject==other.pplobject) Some(0)  else 
        if (pplobject strictly_contains other.pplobject) Some(1) else 
          if (other.pplobject strictly_contains pplobject) Some(-1)
          	else None
    case _ => None
  }
  
  override def equals(other: Any): Boolean = other match {
    case other: PPLCPolyhedronRefl => pplobject.equals(other.pplobject)
    case _ => false
  }

  override def hashCode: Int = pplobject.hashCode
  
  override def toString : String = pplobject.toString
  
  /**
   * Converts a sequence of homogeneous and in-homogeneous coefficients into a `Linear_Expression`, which
   * is a PPL internal object.
   * @param coeff the homogeneous coefficients.
   * @param known the in-homogeneous coefficient.
   */
  private def toPPLLinearExpression(coeff:Array[Double], known:Double): Linear_Expression = {
    var le : Linear_Expression = new Linear_Expression_Coefficient(new Coefficient(known.toInt))
	for (i <- 0 to (coeff.length - 1)) {
	  le = le.sum ( (new Linear_Expression_Variable(new Variable(i)).times(new Coefficient(coeff(i).toInt)) ))
	}
    return le
  }
}

/**
 * This is the factory for ``PPLBoxDouble`` properties.
 */
object PPLCPolyhedronRefl extends NumericalDomain[PPLCPolyhedronRefl] {  

  def full(n: Int): PPLCPolyhedronRefl = {
    val pplpolyhedron = new C_Polyhedron(n, Degenerate_Element.UNIVERSE)
	new PPLCPolyhedronRefl(pplpolyhedron)
  }
  
  def empty(n: Int): PPLCPolyhedronRefl = {
    val pplpolyhedron = new C_Polyhedron(n, Degenerate_Element.EMPTY)
    new PPLCPolyhedronRefl(pplpolyhedron)        
  }  
}
