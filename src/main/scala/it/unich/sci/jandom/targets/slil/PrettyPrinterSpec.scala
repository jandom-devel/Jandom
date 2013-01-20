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
package targets.slil

import it.unich.sci.jandom.domains.AbstractProperty
import it.unich.sci.jandom.targets.Environment
/**
 * This is a class containing parameters for pretty printing a SLILProgram with annotations.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class PrettyPrinterSpec(val env: Environment) {
  /**
   * Indent is a function which takes an indentation level, and returns a set of whitespaces (or other string)
   * which realizes this indentation. The default is to print two spaces for each indentation level. 
   */
  private[slil] var indent: Int => String = { level => " " * level * 2}
  
  /**
   * Decorator is a function which takes annotation (of any kind), and returns its string representation.
   * The default simply prints the annotation between brackets.   
   */
  
  private[slil] def decorator (p: AbstractProperty): String = ('[' + p.mkString(env.variables).mkString(" , ") + ']')
}


/** 
 * The companion object for PrettyPrinterParameters
 */
object PrettyPrinterSpec {
  
  /** 
   * Builds a pretty printer specification with all the default values
   */
  def apply(env: Environment) = new PrettyPrinterSpec(env)  
}   
  

