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

package it.unich.sci.jandom;

object Jandom extends App {
  var i, j: domains.BoxDouble = null;
  i = domains.BoxDouble(Array(1, 2), Array(3, 4));
  println(i);
  j = domains.BoxDouble(Array(0, 3), Array(3, 5));
  println(j);
  println(i union j)
  println(i.linearAssignment(1,Array(1,2),3))
  
  var ii: IntInterval = null
  ii = IntInterval.void
  println(ii)
  ii = IntInterval.full
  println(ii)
  ii = IntInterval(3, 4)
  println(ii)
}
