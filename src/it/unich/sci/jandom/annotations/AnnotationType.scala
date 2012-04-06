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
package it.unich.sci.jandom.annotations

import it.unich.sci.jandom.targets.Target

/**
 * This is the abstract classes used for typing annotations in blackboards and 
 * specifying default values. Each annotation we want to put in the blackboard
 * needs to define an object derived from AnnotationType.
 * @author Gianluca Amato <g.amato@unich.it>
 *
 */
abstract class AnnotationType {
  /**
   * The type of annotation
   */
  type T
    
  /** 
   * The default value for this annotation type.
   */
  val defaultValue: T
  
  /**
   * The string version of an AnnotationType is the simple class name (i.e.
   * the class name without packages)
   */
  override def toString = getClass.getSimpleName
}
