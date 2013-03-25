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

package it.unich.sci

/**
 * ''Jandom'' is a framework for developing static analysis tools based on [[http://www.di.ens.fr/~cousot/AI/ ''Abstract Interpretation'']].
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
package object jandom {
  /**
   * This is the major version of Jandom. The API is considered to be stable within the same major version, but only
   * from major_version 1 onwards. 
   */
  val majorVersion = 0
  
  /**
   * This is the minor version of Jandom. An increase in minor version suggests the introduction of new backward 
   * compatible features.
   */
  val minorVersion = 2
  
  /**
   * This is the patchlevel of Jandom.
   */
  val patchlevelVersion = 2
  
  /**
   * Suffix version
   */
  val suffixVersion = "git"
  
  /**
   * This is a string representation of Jandom's version
   */
  val version = majorVersion + "." + minorVersion + "." + patchlevelVersion + "-" + suffixVersion
  
  /**
   * This is the name of the software
   */
  val softwareName = "Jandom"
}
