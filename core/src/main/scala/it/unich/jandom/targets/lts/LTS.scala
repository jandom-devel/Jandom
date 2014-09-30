/**
 * Copyright 2013, 2014 Gianluca Amato <gamato@unich.it>
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

package it.unich.jandom.targets.lts

import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.targets.{ Environment, Target }
import it.unich.jandom.targets.Annotation
import it.unich.jandom.domains.CartesianFiberedProperty
import it.unich.jandom.domains.DimensionFiberedDomain
import it.unich.jandom.domains.DimensionFiberedProperty

/**
 * The class for the target of Linear Transition Systems.
 * @param locations the locations which makes the LTS
 * @param transitions the transitions which makes the LTS
 * @param env the environment of the LTS
 * @param regions the regions which are part of the LTS description
 * @author Gianluca Amato <gamato@unich.it>
 *
 */

case class LTS(val locations: IndexedSeq[Location], val transitions: Seq[Transition], val env: Environment, val regions: Seq[Region] = Seq()) extends Target[LTS] {

  // fill locations with their numerical index.. this is used to speed up execution
  locations.zipWithIndex.foreach { case (loc, index) => loc.id = index }

  // set s to the number of locations. A Seq is not guaranteed to have a fast size method, hence we
  // cache the result
  private[this] val s = locations.size

  type ProgramPoint = Location
  type DomainBase = NumericalDomain

  def size = s
  val lastPP = None

  class LTSAnnotation[Property] extends Annotation[ProgramPoint, Property] {
    private val buffer = Array.fill[Option[Property]](s)(None)
    def get(key: ProgramPoint) = buffer(key.id)
    def iterator = buffer.indices.filter(buffer(_) != None).map { i => (locations(i), buffer(i).get) }.toIterator
    def +=(kv: (ProgramPoint, Property)): this.type = {
      buffer(kv._1.id) = Some(kv._2)
      return this
    }
    def -=(key: ProgramPoint): this.type = {
      buffer(key.id) = None
      return this
    }

    override def empty = new LTSAnnotation[Property]
  }

  override def getAnnotation[Property] = new LTSAnnotation[Property]

  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property] = {
    // build widening and narrowing for each program point
    val widenings = locations map params.wideningFactory
    val narrowings = locations map params.narrowingFactory

    // build an empty property.. it is used several times, so we speed execution
    val empty = params.domain.bottom(env.size)
    val initRegion = regions find { _.name == "init" }

    // this are the initial non empty states of the LTS
    val initial = initRegion match {
      case Some(Region(_, Some(initloc), initcond)) =>
        locations map { loc => if (loc == initloc) initcond.analyze(params.domain.top(env.size)) else empty }
      case _ =>
        locations map { loc => (params.domain.top(env.size) /: loc.conditions) {
          (prop, cond) => cond.analyze(prop)
        } }
      }

    var next = initial
    var current = initial

    do {
      current = next
      next = for (loc <- locations) yield {
        val propnew = for (t <- loc.incomings) yield t.analyze(current(t.start.id))
        val unionednew = propnew.fold(empty)(_ union _)
        widenings(loc.id)(current(loc.id), unionednew)
      }
    } while (current != next)

    do {
      current = next
      next = for (loc <- locations) yield {
        val propnew = for (t <- loc.incomings) yield t.analyze(current(t.start.id))
        val unionednew = (propnew.fold(empty)(_ union _)) union initial(loc.id)
        narrowings(loc.id)(current(loc.id), unionednew)
      }
    } while (current != next)
    val ann = getAnnotation[params.Property]
    locations.foreach { loc => ann(loc) = current(loc.id) }
    return ann
  }

  def mkString[U <: DimensionFiberedProperty[U]](ann: Annotation[ProgramPoint, U]): String = {    
    (for ( (loc,prop) <- ann ) yield loc.name + " => "+ prop.mkString(env.variables)).mkString(", ")
  }

  override def toString = locations.mkString("\n") + "\n" + (transitions map { _.mkString(env.variables) }).mkString("\n") + "\n" +
    (regions map { _.mkString(env.variables) }).mkString("\n")
}
