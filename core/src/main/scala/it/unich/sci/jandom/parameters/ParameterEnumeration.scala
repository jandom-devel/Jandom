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

package it.unich.sci.jandom.parameters

abstract class ParameterEnumeration extends Enumeration with Parameter[ParameterValue] { 
  val default: Value
  val name: String
  val shortName: String
  val description: String  
  class Val(val name: String, val description: String) extends super.Val(name) with ParameterValue
  protected final def Value(name: String = "", description : String = ""): Value = new Val(name, description)
  def enabledValues =  values.toSeq.asInstanceOf[Seq[Val]] 
}
