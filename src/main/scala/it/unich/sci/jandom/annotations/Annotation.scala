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

package it.unich.sci.jandom
package annotations

import targets.Target

/**
 * An annotation is a value we can attach to compiled programs (''targets'' in ''Jandom'''s terminology). 
 * Therefore, annotations are parametric w.r.t. a [[it.unich.sci.jandom.targets.Target]] and an 
 * [[it.unich.sci.jandom.annotations.AnnotationType]]. An annotation contains both a global value
 * and values attached to the single program points. The latter are kept in a field of type
 * [[it.unich.sci.jandom.annotations.PerProgramPointAnnotation]] which is built, when needed, by
 * a factory  of type [[it.unich.sci.jandom.annotations.PerProgramPointAnnotationBuilder]].
 * @tparam Tgt the target type for this annotation.
 * @tparam Ann the annotation type class for this annotation.
 * @param t the target for this annotation.
 * @param ann the annotation type for this annotation.
 * @param maker the factory for [[it.unich.sci.jandom.annotations.PerProgramPointAnnotation]]. It is generally
 * provided implicitly by the companion object of `Tgt`.
 * @return an annotation
 * @author Gianluca Amato <amato@sci.unich.it>
 */
class Annotation[Tgt <: Target, Ann <: AnnotationType](t: Tgt, ann: Ann)(implicit maker: PerProgramPointAnnotationBuilder[Tgt]) {
  
  /**
   * The field which contains the values attached to the single program points.
   */
  private[this] var perPP: PerProgramPointAnnotation[Tgt, Ann] = null

  /**
   * The global value.
   */
  var global: Ann#T = _

  /**
   * Returns the corresponding  [[it.unich.sci.jandom.annotations.PerProgramPointAnnotation]],
   * eventually building it if needed.
   */
  def toPPAnnotation = {
    if (perPP == null) {
      perPP = maker.apply[Ann](t, ann)
    }
    perPP
  }

  /**
   * Given a program point, returns the value for the annotation at that program point.
   */
  def apply(pp: Tgt#ProgramPoint): Ann#T = {
    if (perPP == null) {
      perPP = maker.apply[Ann](t, ann)
    }
    perPP(pp)
  }

  /**
   * Given a program point and a value, updates the  annotation.
   */
  def update(pp: Tgt#ProgramPoint, v: Ann#T) {
    if (perPP == null) {
      perPP = maker.apply[Ann](t, ann)
    }
    perPP(pp) = v
  }

  override def toString = "global: " + global + "\nperPP:\n" + perPP
}

