/**
 * Copyright 2013 Gianluca Amato
 * 
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
 */

package it.unich.sci.jandom.targets

import org.scalatest.FunSuite

import it.unich.sci.jandom.domains.BoxDouble

/** 
 * The Test Suite for the LinearAssignment class
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class LinearAssignmentSuite extends FunSuite {
  
  val env = Environment("v1","v2")
    
  test("simple linear form assignments") {
    val d = BoxDouble.full(env.size)
    val la1 = LinearAssignment( 0, LinearForm.fromCoefficient(0) )
    val d1 = la1.analyze(d)
    expectResult ( BoxDouble(Array(0,Double.NegativeInfinity), Array(0,Double.PositiveInfinity))) { d1 }
    val la2 = LinearAssignment( 1, LinearForm.fromCoefficient(1) )
    val d2 = la2.analyze(d1)
    expectResult ( BoxDouble(Array(0,1), Array(0,1))) { d2 }    
  }	
}