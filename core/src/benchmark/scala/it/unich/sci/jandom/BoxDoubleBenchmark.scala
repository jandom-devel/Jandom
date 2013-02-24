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
package it.unich.sci.jandom

import com.google.caliper.SimpleBenchmark
import parma_polyhedra_library._
import it.unich.sci.jandom.domains.BoxDouble
import it.unich.sci.jandom.domains.PPLBoxDouble
import it.unich.sci.jandom.domains.PPLProperty
import it.unich.sci.jandom.domains.PPLDomain
import it.unich.sci.jandom.domains.PPLPropertyMacros


/**
 * This benchmark compare some operations on the interval domain. This 
 * shows that PPL is very slow in Jandom, and Reflexive PPL is even
 * slower.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

class BoxDoubleBenchmark extends SimpleBenchmark {
  private val numvars = 100
  private val numpoints = 10

  System.loadLibrary("ppl_java")
  Parma_Polyhedra_Library.initialize_library()

  def timePPL(reps: Int) {
    for (iter <- 1 to reps) {
      // create an empty box	
      val db = new Double_Box(numvars, Degenerate_Element.EMPTY)

      // initialize a list of linear form (one for each variable)
      val vars = new Array[Linear_Expression_Variable](numvars)
      for (v <- 0 until numvars) vars(v) = new Linear_Expression_Variable(new Variable(v))

      // initialize the linear form x_1 +  ... + x_n
      val diagonal = vars.reduceRight[Linear_Expression](_.sum(_))
      val gs = new Generator_System()
      for (i <- 1 to numpoints) {
        val point = Generator.point(diagonal times (new Coefficient(i)), new Coefficient(1))
        gs.clear
        gs.add(point)
        val point_box = new Double_Box(gs)
        db.upper_bound_assign(point_box)
      }
    }
  }

  def timePPL2(reps: Int) {
    for (iter <- 1 to reps) {
      val db = new Double_Box(numvars, Degenerate_Element.EMPTY)
      val v0 = new Variable(0)
      val vlast = new Variable(numvars - 1)
      val expr = (new Linear_Expression_Variable(v0)) sum (new Linear_Expression_Variable(vlast))
      val gs = new Generator_System()
      val point = Generator.point(expr, new Coefficient(1))
      gs.add(point)
      db.upper_bound_assign(new Double_Box(gs))
      val denominator = new Coefficient(1)
      for (i <- 1 to numpoints) {
        val dbnew = new Double_Box(db)
        dbnew.affine_image(v0, expr, denominator)
        db.upper_bound_assign(dbnew)
      }
    }
  }

  def timeJandomNoPPLOptimized(reps: Int) {
    for (iter <- 1 to reps) {
      var db = BoxDouble.empty(numvars)
      for (i <- 1 to numpoints) {
        val point = Array.fill(numvars)(i.toDouble)
        db = db union BoxDouble(point)
      }
      println(db)
    }
  }

  def timeJandomNoPPL(reps: Int) {
    for (iter <- 1 to reps) {
      var db = BoxDouble.empty(numvars)
      val zero = Array.fill(numvars)(0.0)
      val full = BoxDouble.full(numvars)
      for (i <- 1 to numpoints) {
        val point = (0 until numvars).foldLeft(full) { (box, v) => box.linearAssignment(v, zero, i.toDouble) }            
        db = db union point
      }
    }
  }
  
  def timeJandomPPL(reps: Int) {
    for (iter <- 1 to reps) {
      var db = PPLBoxDouble.empty(numvars)
      val zero = Array.fill(numvars)(0.0)
      val full = PPLBoxDouble.full(numvars)
      for (i <- 1 to numpoints) {
        val point = (0 until numvars).foldLeft(full) { (box, v) => box.linearAssignment(v, zero, i.toDouble) }            
        db = db union point
      }
    }
  }
  
  def timeJandomPPLReflexive(reps: Int) {
    for (iter <- 1 to reps) {
      val domain = new PPLDomain[Octagonal_Shape_double]
      var db = domain.empty(numvars)
      val zero = Array.fill(numvars)(0.0)
      val full = domain.full(numvars)
      for (i <- 1 to numpoints) {
        val point = (0 until numvars).foldLeft(full) { (box, v) => box.linearAssignment(v, zero, i.toDouble) }            
        db = db union point
      }
    }
  }
  
  def timeJandomPPLMacro(reps: Int) {
    for (iter <- 1 to reps) {
      val domain = PPLPropertyMacros.PPLDomain[Double_Box]
      var db = domain.empty(numvars)
      val zero = Array.fill(numvars)(0.0)
      val full = domain.full(numvars)
      for (i <- 1 to numpoints) {
        val point = (0 until numvars).foldLeft(full) { (box, v) => box.linearAssignment(v, zero, i.toDouble) }            
        db = db union point
      }
    }
  }
}
