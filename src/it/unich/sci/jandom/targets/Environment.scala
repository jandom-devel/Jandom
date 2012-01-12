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
package it.unich.sci.jandom.targets

import scala.collection.mutable.LinkedHashMap

/**
 * The class Environment represents an environment at a certain point in a program, i.e. a list of variables 
 * and corresponding types.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

class Environment {
	private var currindex: Int = 0
    private var variables = new LinkedHashMap[String,Int]()   
	
	/**
	 * Add a new variable to the environment.
	 * @param typeVar the type of the variable
	 * @param name the name of the variable
	 */
	def addVariable(name: String)  {
	  currindex += 1
	  variables += (name -> currindex)
	}
	
	/**
	 * Get the variable code from the environment of add a new variable if it does not exists
	 * @param typeVar the type of the variable
	 * @param name the name of the variable
	 */
	def getVariableOrAdd(name: String) =  variables.get(name) match {
      case Some(n) => n
      case None => {
        currindex += 1
        variables += (name -> currindex)        
        currindex 
      }
    }    
	
	def getVariableName(i: Int) = variables.keys.toSeq(i-1) 
	
	def getVariableNames = variables.keys
	
	def getNumVariables = currindex
}

object Environment {
  def apply(names: String*) = {
    val env = new Environment()
    names foreach { env.addVariable(_)}
    env
  }
}
