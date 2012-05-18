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
package annotations

import targets.Target

/**
 * This is the class for annotation. An annotation is parameteric w.r.t.
 * a target and an annotation type. An annotation contain a global objecet and
 * "per program point" annotations which are built when needed using an 
 * object of class PerProgramPointAnnotationBuilder.
 * @tparam Tgt the target for the annotation
 * @param ann the annotation type
 * @maker an object which build per-programpoint annotations
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class Annotation[Tgt <: Target, Ann <: AnnotationType](t:Tgt, ann: Ann) (implicit maker: PerProgramPointAnnotationBuilder[Tgt]) {
  type T = Ann#T  
  
  /**
   * The "per-program point" annotation
   */
  private[this] var perPP: PerProgramPointAnnotation[Tgt,Ann] = null
  
  /**
   * The global annotation
   */
  var global: T = _    
  
  /**
   * Returns the PerPerProgramPointAnnotation corresponding to this.
   */
  def toPPAnnotation = {
	if (perPP == null) {
      perPP = maker.apply[Ann](t, ann)
    }
   	perPP
  }
  
  /**
   * Given a program point, returns the corresponding annotation.
   */
  def apply(pp: Tgt#ProgramPoint): T = {
    if (perPP == null) {
      perPP = maker.apply[Ann](t, ann)
    }
    perPP(pp)
  }
  
  /**
   * Given a program point and a value, update the corresponding annotation.
   */
  def update(pp: Tgt#ProgramPoint, v: T) {
    if (perPP == null) {
      perPP = maker.apply[Ann](t, ann)
    }
    perPP(pp) = v
  }

  override def toString = "global: " + global + "\nperPP:\n" + perPP
}

