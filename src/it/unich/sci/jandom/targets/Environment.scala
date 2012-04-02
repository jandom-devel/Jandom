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
 * The class Environment represents an environment at a certain point in a program. At the moment
 * this means essentially a correspondence between a variable name and a variable numerical index.
 * Later, we will add also types (integer, floating points, etc...) to the environment.
 *  
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

class Environment {	
    private val variables = new scala.collection.mutable.ArrayBuffer[String]()
    private val nameHash = new LinkedHashMap[String,Int]()
    
	/**
	 * Add a new binding to the environment. If a variable with the same name is already in the environment,
	 * errors may happen.
	 * @param name the name of the variable
	 * @return the index of the new binding (first variable has index 0)
	 */
	def addBinding(name: String): Int = {      
  	  variables += name
	  nameHash += name -> (variables.size - 1)
	  return variables.size -1 
	}
	
	/**
	 * Get a binding from the environment based on the name.
	 * @param name the name of the variable
	 * @return the index of the binding for name
	 */
	def getBinding(name: String): Option[Int] = nameHash.get(name)			
		   	
	/**
	 * Get a binding from the environment based on the name, or add a new binding
	 * @param name the name of the variable
	 * @return the index of the binding for name
	 */
	def getBindingOrAdd(name:String) : Int = nameHash.get(name) match {
	  case Some(v) => v
	  case None => addBinding(name)
	}

	/**
	 * Get a binding from the environment based on the name. 
	 * @param name the name of the variable
	 * @return the variable with the given name in the environment
	 * @throw NoSuchElementException if there is no variables with such an index
	 */
	def apply(name: String): Int = nameHash(name)
		
	/**
	 * Get a variable name from the environment based on the binding.
	 * @param index the index of the variable
	 * @return the variable name with the given index in the environment
	 * @throw NoSuchElementException if there is no variables with such an index
	 */
	def apply(index: Int): String = variables(index)
		
	/**
	 * Returns the name of variables in the environment
	 * @return the variable with the given index in the environment
	 */
	def getNames: Iterable[String] = variables.toIterable
	  
	/**
	 * Returns the size of the environment, i.e. the number of bindings
	 * @return the size of the environment
	 */
	def size = variables.size
}

object Environment {
  /**
   * Returns a new environment with new variables whose names are given as arguments
   * @param names the name of variables in the new environment
   * @return a new environment with new variables of given names
   */
  def apply(names: String*) = {
    val env = new Environment()
    names foreach { env.addBinding(_) }
    env
  }
}
