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
package it.unich.sci.jandom.targets

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
 * The Test Suite for the Variable class
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class VariableSuite extends FunSuite with Checkers {
	test ("standard constructors") {
	  val v1 = new Variable("x")
	  val v2 = new Variable("x")
	  expect(false) { v1==v2 }	  
	}
	
	test ("companion object constructors") {
	  val v1 = Variable("x");
	  val v2 = Variable("x");
	  expect(false) { v1==v2 };
	}
}
