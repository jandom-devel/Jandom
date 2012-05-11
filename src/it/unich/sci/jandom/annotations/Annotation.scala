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
 * a target and an annotation type.
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class Annotation[Tgt <: Target, Ann <: AnnotationType](t:Tgt, ann: Ann) (implicit maker: PerProgramPointAnnotationBuilder[Tgt]) {
  type T = Ann#T
  private[this] var perPP: PerProgramPointAnnotation[Tgt,Ann] = null
  var global: T = _    
  
  def toPPAnnotation = {
	if (perPP == null) {
      perPP = maker.apply[Ann](t, ann)
    }
   	perPP
  }
  
  def apply(pp: Tgt#ProgramPoint): T = {
    if (perPP == null) {
      perPP = maker.apply[Ann](t, ann)
    }
    perPP(pp)
  }
  
  def update(pp: Tgt#ProgramPoint, v: T) {
    if (perPP == null) {
      perPP = maker.apply[Ann](t, ann)
    }
    perPP(pp) = v
  }

  override def toString = "global: " + global + "\nperPP:\n" + perPP
}

