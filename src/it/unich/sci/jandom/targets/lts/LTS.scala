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
package targets.lts

import domains.NumericalProperty
import targets.{ Environment, Parameters, Target }
import annotations._
import widenings.Widening
import narrowings.Narrowing
import scala.collection.mutable.{Map, ArrayBuffer}

/**
 * The class for the target of Linear Transition Systems.
 * @param locations the locations which makes the LTS
 * @param transitions the transitions which makes the LTS
 * @param env the environment of the LTS
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */

class LTS(private val locations: IndexedSeq[Location], private val transitions: Seq[Transition], private val env: Environment) extends Target {

  // fill locations with their numerical index.. this is used to speed up execution
  locations.zipWithIndex.foreach { case (loc, index) => loc.id = index }

  // set s to the number of locations. A Seq is not guaranteed to have a fast size method, hence we
  // cache the result
  private[this] val s = locations.size

  type ProgramPoint = Location
  type Tgt = LTS

  def size = s
  
  class Annotation[Property] extends Map[ProgramPoint, Property]  {
	private val buffer = Array.fill[Option[Property]](s)(None)
	def get(key: ProgramPoint) = buffer(key.id)
	def iterator = buffer.indices.filter( buffer(_) != None).map { i => (locations(i),buffer(i).get ) }.toIterator
	def +=(kv: (ProgramPoint, Property)): this.type = {	  
	  buffer(kv._1.id) = Some(kv._2)
	  return this
	}
	def -=(key: ProgramPoint): this.type = {
	  buffer(key.id) = None
	  return this
	}
	override def empty = new Annotation[Property]
  } 
  
  def getAnnotation[Property] = new Annotation[Property]
  
  def analyze[Property <: NumericalProperty[Property]](params: Parameters[Property]): Annotation[Property] = {
    // build widening and narrowing for each program point    
    val widenings = locations map params.wideningFactory
    val narrowings = locations map params.narrowingFactory

    // build an empty property.. it is used several times, so we speed execution    
    val empty = params.domain.empty(env.size)

    var current = locations map { _ => empty }
    var next = locations map { loc =>
      (params.domain.full(env.size) /: loc.conditions) {
        (prop, cond) => cond.analyze(prop)
      }
    }

    while (current != next) {
      current = next
      next = for (loc <- locations) yield {
        val propnew = for (t <- loc.incomings) yield t.analyze(current(t.start.id))
        val unionednew = propnew.fold(empty)(_ union _)
        widenings(loc.id)(current(loc.id), unionednew)
      }
    }

    current = null
    while (current != next) {
      current = next
      next = for (loc <- locations) yield {
        val propnew = for (t <- loc.incomings) yield t.analyze(current(t.start.id))
        val unionednew = propnew.fold(empty)(_ union _)
        narrowings(loc.id)(current(loc.id), unionednew)
      }
    }
    val ann = new Annotation[Property]	    	    
    locations.foreach { loc => ann(loc) = current(loc.id) }
    return ann
  }

  override def toString = locations.mkString("\n") + "\n" + transitions.mkString("\n")
}

/**
 * The companion object for LTS.
 */
object LTS {
  /**
   * Creates an LTS given locations, transitions and environment.
   */
  def apply(locations: IndexedSeq[Location], transitions: Seq[Transition], env: Environment) = new LTS(locations, transitions, env)
}
