/** This class implements integer interval arithmetic
 *
 * Copyright 2011 Gianluca Amato
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

/**
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class IntInterval(val low:Int, val high:Int) {
	def isVoid = low > high
	def isFull = (low == IntInterval.NEGINF) && (high == IntInterval.POSINF)	
	override def toString: String = "[" + low + "," + high + "]"
}

object IntInterval {
	val NEGINF = Int.MinValue
	val POSINF = Int.MaxValue
	def apply(i: Int) = new IntInterval(i,i)
	def apply(low: Int, high: Int) = new IntInterval(low, high)
	def void = new IntInterval(POSINF, NEGINF)
	def full = new IntInterval(NEGINF, POSINF)	  
}