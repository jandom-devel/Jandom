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

package it.unich.sci.jandom.domains

import org.scalatest.FunSuite

import it.unich.sci.jandom.domains.numerical.LinearForm

/**
 * The Test Suite for the LinearForm class
 * @author Gianluca Amato <amato@sci.unich.it>
 */

class LinearFormSuite extends FunSuite {

  test("LinearForm companion object standard constructor") {
    val lf = LinearForm(1,2,-1)
    expectResult("1+2*v0-v1") { lf.toString }
    val lf2 = LinearForm(1,0,3)
    expectResult("1+3*v1") { lf2.toString }
  }

  test("LinearForm companion object v constructor") {
    var lf = LinearForm.v[Int](1)
    expectResult("v1") { lf.toString }
  }

  test("LinearForm companion object sparse constructor") {
    val lf1 = LinearForm(0,0,2)
    val lf2 = LinearForm(0, 1 -> 2)
    expectResult(lf1) { lf2 }
  }

  test("LinearForm companion addition") {
    val lf1 = LinearForm(1,2,-1)
    val lf2 = LinearForm(1,0,3)
    val lf3 = LinearForm(2,2,2)
  	expectResult(lf3) { lf1+lf2 }
  }

  test("Equality") {
    val lf1 = LinearForm(1,0,3)
    val lf2 = LinearForm(1,0,3)
    expectResult(lf1) { lf2 }
    val lf3 = LinearForm(1,0,1)
    expectResult(false) (lf1 == lf3)
  }
}