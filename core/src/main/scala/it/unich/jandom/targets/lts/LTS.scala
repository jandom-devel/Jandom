/**
 * Copyright 2013, 2014, 2016 Gianluca Amato <gianluca.amato@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.jandom.targets.lts

import it.unich.jandom.domains.DimensionFiberedProperty
import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.targets._
import it.unich.jandom.targets.parameters._
import it.unich.scalafix.finite.GraphEquationSystem

/**
 * The class for the target of Linear Transition Systems.
 * @param locations the locations which makes the LTS
 * @param transitions the transitions which makes the LTS
 * @param env the environment of the LTS
 * @param regions the regions which are part of the LTS description
 * @author Gianluca Amato <gianluca.amato@unich.it>
 */

case class LTS(val name: String, val locations: IndexedSeq[Location], val transitions: Seq[Transition], val env: Environment, val regions: Seq[Region] = Seq()) extends Target[LTS] {

  type ProgramPoint = Location
  type DomainBase = NumericalDomain

  // fill locations with their numerical index.. this is used to speed up execution.
  locations.zipWithIndex.foreach { case (loc, index) => loc.id = index }

  // A Seq is not guaranteed to have a fast size method, hence we cache the result.
  val numlocs: Int = locations.size

  val lastPP: Option[Location] = None

  val entryPP: Seq[Location] = regions.find { _.name == "init" } match {
    case Some(Region(_, Some(loc), _)) => Seq(loc)
    case Some(Region(_, None, _)) => throw new IllegalArgumentException("An init region should provide a starting state")
    case None => locations
  }

  private lazy val dfsVisit: Unit = {
    var c: Int = numlocs

    def dfs(loc: Location): Unit = {
      loc.visited = true
      for (edge <- loc.outgoing; dest = edge.end; if !dest.visited) dfs(dest)
      loc.dfo = c
      c -= 1
    }
    for (loc <- locations) loc.visited = false
    for (loc <- entryPP; if !loc.visited) dfs(loc)
  }

  def dfo(l: Location) = {
    dfsVisit
    l.dfo
  }

  def isRetreating(t: Transition) = {
    dfsVisit
    t.end.dfo <= t.start.dfo
  }

  def isJoinNode(l: Location) = {
    l.incoming exists { isRetreating(_) }
  }

  class LTSAnnotation[Property] extends Annotation[ProgramPoint, Property] {
    private val buffer = Array.fill[Option[Property]](numlocs)(None)
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

  /**
   * Converts the LTS into a graph in the DOT language.
   */
  def toDot: String = {
    import org.apache.commons.lang3.StringEscapeUtils

    val builder = new StringBuilder()
    builder ++= "digraph {\n"
    val initState = regions find { _.name == "init" } flatMap { _.state }
    if (initState.isDefined)
      builder ++= s"""  "${StringEscapeUtils.escapeJava(initState.get.name)}" [shape=doublecircle]\n"""
    for (t <- transitions) {
      builder ++= s"""  "${StringEscapeUtils.escapeJava(t.start.name)}" -> "${ StringEscapeUtils.escapeJava(t.end.name)}" [label="${StringEscapeUtils.escapeJava(t.name)}"]\n"""
    }
    builder ++= "}\n"
    builder.toString
  }

  /**
   * Converts an LTS into an equation system, given a numerical domain.
   */
  def toEQS(dom: NumericalDomain): GraphEquationSystem[Location, dom.Property, Either[Transition, Location]] = {
    type Edge = Either[Transition, Location]

    implicit val scalafixDomain = dom.ScalaFixDomain

    // build an empty property.. it is used several times, so we speed execution
    val empty = dom.bottom(env.size)
    val initRegion = regions find { _.name == "init" }

    // this are the initial non empty states of the LTS
    val (startrho, inputlocs) = initRegion match {
      case Some(Region(_, Some(initloc), initcond)) =>
        ({ loc: Location => if (loc == initloc) initcond.analyze(dom.top(env.size)) else empty }, Set(initloc))
      case _ =>
        ({ loc: Location =>
          (dom.top(env.size) /: loc.conditions) {
            (prop, cond) => cond.analyze(prop)
          }
        }, locations)
    }
    val edges: Set[Edge] = ((transitions map { Left(_) }) ++ (inputlocs map { Right(_) })).toSet
    GraphEquationSystem[Location, dom.Property, Edge](
      unknowns = locations,
      inputUnknowns = inputlocs.toSet,
      edgeAction = { (rho: Location => dom.Property) =>
        e: Either[Transition, Location] =>
          e match {
            case Left(t) => t.analyze(rho(t.start))
            case Right(l) => startrho(l)
          }
      },
      source = { _ match { case Left(t) => Some(t.start); case Right(l) => None } },
      target = { _ match { case Left(t) => t.end; case Right(l) => l } },
      ingoing = { l: Location => Right(l) :: (l.incoming map { Left(_) }) },
      outgoing = { l: Location =>  l.outgoing map { Left(_) } },
      initial = startrho)
  }

  override def getAnnotation[Property] = new LTSAnnotation[Property]

  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property] = {
    // build widening and narrowing for each program point
    val widenings = locations map { l =>
      if (params.wideningLocation == WideningNarrowingLocation.All ||
        (params.wideningLocation == WideningNarrowingLocation.Loop && isJoinNode(l)))
        Some(params.wideningAssignment(l))
      else
        None
    }
    val narrowings = locations map { l =>
      if (params.narrowingLocation == WideningNarrowingLocation.All ||
        (params.narrowingLocation == WideningNarrowingLocation.Loop && isJoinNode(l)))
        Some(params.narrowingAssignment(l))
      else
        None
    }

    // build an empty property.. it is used several times, so we speed execution
    val empty = params.domain.bottom(env.size)
    val initRegion = regions find { _.name == "init" }

    // this are the initial non empty states of the LTS
    val initial = initRegion match {
      case Some(Region(_, Some(initloc), initcond)) =>
        locations map { loc => if (loc == initloc) initcond.analyze(params.domain.top(env.size)) else empty }
      case _ =>
        locations map { loc =>
          (params.domain.top(env.size) /: loc.conditions) {
            (prop, cond) => cond.analyze(prop)
          }
        }
    }
    val ann = getAnnotation[params.Property]

    if (params.iterationStrategy == IterationStrategy.Kleene) {
      var next = initial
      var current = initial

      do {
        current = next
        next = for ((loc, w) <- locations zip widenings) yield {
          val propnew = for (t <- loc.incoming) yield t.analyze(current(t.start.id))
          val unionednew = propnew.fold(empty)(_ union _)
          if (w.isEmpty) unionednew else w.get(current(loc.id), unionednew)
        }
      } while (current != next)

      do {
        current = next
        next = for ((loc, n) <- locations zip narrowings) yield {
          val propnew = for (t <- loc.incoming) yield t.analyze(current(t.start.id))
          val unionednew = current(loc.id) intersection (propnew.fold(empty)(_ union _) union initial(loc.id))
          if (n.isEmpty) unionednew else n.get(current(loc.id), unionednew)
        }
      } while (current != next)
      locations.foreach { loc => ann(loc) = current(loc.id) }

    } else {
      val current = initial.toBuffer
      val workList = collection.mutable.Queue[Int]()

      params.log("Beginning ascending chain\n")
      workList ++= 0 until numlocs
      while (!workList.isEmpty) {
        val locid = workList.dequeue()
        val loc = locations(locid)
        val w = widenings(locid)
        val propnew = for (t <- loc.incoming) yield t.analyze(current(t.start.id))
        val unionednew = propnew.fold(empty)(_ union _)
        params.log(s"Node: ${loc.name} Oldvalue: ${current(loc.id).mkString(env.variables)} Newinput: ${unionednew.mkString(env.variables)}")
        val newvalue = if (w.isEmpty) unionednew else w.get(current(locid), unionednew)
        params.log(s" Newvalue: ${newvalue.mkString(env.variables)}\n")
        if (newvalue > current(locid)) {
          current(locid) = newvalue
          for (t <- loc.outgoing) { if (!workList.contains(t.end.id)) workList.enqueue(t.end.id) }
        }
      }

      params.log("Beginning descending chain\n")

      workList ++= 0 until numlocs
      while (!workList.isEmpty) {
        val locid = workList.dequeue()
        val loc = locations(locid)
        val n = narrowings(locid)
        val propnew = for (t <- loc.incoming) yield t.analyze(current(t.start.id))
        val unionednew = current(locid) intersection (propnew.fold(empty)(_ union _) union initial(locid))
        params.log(s"Node: ${loc.name} Oldvalue: ${current(loc.id).mkString(env.variables)} Newinput: ${unionednew.mkString(env.variables)}")
        val newvalue = if (n.isEmpty) unionednew else n.get(current(locid), unionednew)
        params.log(s" Newvalue: ${newvalue.mkString(env.variables)}\n")
        if (newvalue < current(locid)) {
          current(locid) = newvalue
          for (t <- loc.outgoing) { if (!workList.contains(t.end.id)) workList.enqueue(t.end.id) }
        }
      }
      locations.foreach { loc => ann(loc) = current(loc.id) }
    }
    ann
  }

  def mkString[U <: DimensionFiberedProperty[U]](ann: Annotation[ProgramPoint, U]): String = {
    (for ((loc, prop) <- ann) yield loc.name + " : " + prop.mkString(env.variables)).mkString(", \n")
  }

  def mkString = locations.mkString("\n") + "\n" + (transitions map { _.mkString(env.variables) }).mkString("\n") + "\n" +
    (regions map { _.mkString(env.variables) }).mkString("\n")

  override def toString = name

}
