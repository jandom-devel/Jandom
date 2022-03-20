/**
  * Copyright 2013, 2014, 2016, 2018 Gianluca Amato <gianluca.amato@unich.it>
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
import it.unich.jandom.targets.eqs.EQS
import it.unich.jandom.targets.parameters._
import it.unich.scalafix.assignments.InputAssignment
import it.unich.scalafix.finite.GraphEquationSystem
import it.unich.scalafix.lattice.Domain

/**
  * The class for the target of Linear Transition Systems.
  *
  * @param locations   the locations which makes the LTS
  * @param transitions the transitions which makes the LTS
  * @param env         the environment of the LTS
  * @param regions     the regions which are part of the LTS description
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */

class LTS(val name: String, val locations: IndexedSeq[Location], val transitions: Seq[Transition],
          val env: Environment, val regions: Seq[Region] = Seq()) extends Target[LTS] {

  type ProgramPoint = Location
  type DomainBase = NumericalDomain

  private type Edge = Either[Transition, Location]

  // fill locations with their numerical index.. this is used to speed up execution.
  locations.zipWithIndex.foreach { case (loc, index) => loc.id = index }

  // A Seq is not guaranteed to have a fast size method, hence we cache the result.
  val numlocs: Int = locations.size

  /**
    * @inheritdoc
    * An LTS has no last program point, hence the value of `lastPP` is None. In the future,
    * we could use a region to identify the last program point.
    */
  val lastPP: Option[Location] = None

  /**
    * Returns true if `that` is syntactically equal to `this`.
    */
  def syntacticallyEquals(that: LTS): Boolean =
    name == that.name &&
      env == that.env &&
      locations.size == that.locations.size &&
      transitions.size == that.transitions.size &&
      regions.size == that.regions.size &&
      (locations zip that.locations).forall({ l => l._1.syntacticallyEquals(l._2) }) &&
      (transitions zip that.transitions).forall({ t => t._1.syntacticallyEquals(t._2) }) &&
      (regions zip that.regions).forall({ r => r._1.syntacticallyEquals(r._2) })

  /**
    * List of entry program points. If a region called "init" is present, it is the location
    * referred in such a region, otherwise all locations are considered to be entry program points.
    */
  private val entryPP: Seq[Location] = regions.find(_.name == "init") match {
    case Some(Region(_, Some(loc), _)) => Seq(loc)
    case Some(Region(_, None, _)) => throw new IllegalArgumentException("An init region should provide a starting state")
    case None => locations
  }

  /**
    * Computes a Depth-First Search visit of the LTS and determines the depth-first ordering
    * of locations. Since it is a lazy val, successive calls do not cause any recomputing.
    */
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

  /**
    * Returns the index in the depth-first ordering for the node `l`.
    */
  def dfo(l: Location): Int = {
    dfsVisit
    l.dfo
  }

  /**
    * Returns true if `t` is a retreating edge.
    */
  def isRetreating(t: Transition): Boolean = {
    dfsVisit
    t.end.dfo <= t.start.dfo
  }

  /**
    * Returns true if `l` is a join node.
    */
  def isJoinNode(l: Location): Boolean = {
    l.incoming exists isRetreating
  }

  /**
    * An annotation for an LTS. It is implemented with an array of the same length of
    * the number of locations. Therefore, it should be more efficient than a standard
    * hash-table based annotation.
    */
  class LTSAnnotation[Property] extends Annotation[ProgramPoint, Property] {
    private val buffer = Array.fill[Option[Property]](numlocs)(None)

    def get(key: ProgramPoint): Option[Property] = buffer(key.id)

    def iterator: Iterator[(Location, Property)] = buffer.indices.filter(buffer(_).isDefined).map {
      i => (locations(i), buffer(i).get)
    }.toIterator

    def +=(kv: (ProgramPoint, Property)): this.type = {
      buffer(kv._1.id) = Some(kv._2)
      this
    }

    def -=(key: ProgramPoint): this.type = {
      buffer(key.id) = None
      this
    }

    override def empty = new LTSAnnotation[Property]
  }

  /**
    * Converts the LTS into a graph in the DOT language.
    */
  def toDot: String = {
    import org.apache.commons.text.StringEscapeUtils

    val builder = new StringBuilder()
    builder ++= "digraph {\n"
    val initState = regions find (_.name == "init") flatMap (_.state)
    if (initState.isDefined)
      builder ++= s"""  "${initState.get.id}" [shape=doublecircle]\n"""
    for (l <- locations) {
      builder ++= s"""  "${l.id}" [label="${StringEscapeUtils.escapeJava(l.name)}"]\n"""
    }
    for (t <- transitions) {
      builder ++= s"""  "${t.start.id}" -> "${t.end.id}" [label="${StringEscapeUtils.escapeJava(t.name)}"]\n"""
    }
    builder ++= "}\n"
    builder.toString
  }

  /**
    * Converts the LTS into an equation system, given a numerical domain.
    */
  def toEquationSystem(dom: NumericalDomain): GraphEquationSystem[Location, dom.Property, Edge] = {

    implicit val scalafixDomain: Domain[dom.Property] = dom.ScalaFixDomain

    // builds an empty property... it is used several times, so we speed execution
    val empty = dom.bottom(env.size)
    val initRegion = regions find (_.name == "init")

    // this are the initial non empty states of the LTS
    val (startrho, inputlocs) = initRegion match {
      case Some(Region(_, Some(initloc), initcond)) =>
        (InputAssignment.conditional[Location, dom.Property](initloc, initcond.analyze(dom.top(env.size)), empty), Seq(initloc))
      case _ =>
        ( { loc: Location => (loc.conditions foldLeft dom.top(env.size)) ((prop, cond) => cond.analyze(prop)) }, locations)
    }
    GraphEquationSystem[Location, dom.Property, Edge](
      unknowns = locations,
      inputUnknowns = inputlocs.toSet,
      edgeAction = { rho: (Location => dom.Property) => {
        case Left(t) => t.analyze(rho(t.start))
        case Right(l) => startrho(l)
      }
      },
      source = {
        case Left(t) => Some(t.start)
        case Right(_) => None
      },
      target = {
        case Left(t) => t.end
        case Right(l) => l
      },
      ingoing = { l: Location => Right(l) :: (l.incoming map (Left(_))) },
      outgoing = { l: Location => l.outgoing map (Left(_)) },
      initial = startrho
    )
  }

  /**
    * @inheritdoc
    * The LTSAnnotation which is returned by this method is based on arrays, and should be more
    * efficient than the standard hash-table based annotations.
    */
  override def getAnnotation[Property] = new LTSAnnotation[Property]

  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property] = {
    // build widening and narrowing for each program point
    val widenings = locations map { l: Location =>
      if (params.wideningLocation == WideningNarrowingLocation.All ||
        (params.wideningLocation == WideningNarrowingLocation.Loop && isJoinNode(l)))
        Some(params.widening(l))
      else
        None
    }
    val narrowings = locations map { l: Location =>
      if (params.narrowingLocation == WideningNarrowingLocation.All ||
        (params.narrowingLocation == WideningNarrowingLocation.Loop && isJoinNode(l)))
        Some(params.narrowing(l))
      else
        None
    }

    // build an empty property.. it is used several times, so we speed execution
    val empty = params.domain.bottom(env.size)
    val initRegion = regions find {
      _.name == "init"
    }

    // this are the initial non empty states of the LTS
    val initial = initRegion match {
      case Some(Region(_, Some(initloc), initcond)) =>
        locations map {
          loc => if (loc == initloc) initcond.analyze(params.domain.top(env.size)) else empty
        }
      case _ =>
        locations map {
          loc =>
            (loc.conditions foldLeft params.domain.top(env.size)) {
              (prop, cond) => cond.analyze(prop)
            }
        }
    }
    val ann = getAnnotation[params.Property]

    if (params.iterationStrategy == IterationStrategy.Kleene) {
      var next = initial
      var current = initial

      params.log("Beginning ascending chain\n")
      do {
        current = next
        next = for ((loc, w) <- locations zip widenings) yield {
          val propnew = for (t <- loc.incoming) yield t.analyze(current(t.start.id))
          val unionednew = propnew.fold(initial(loc.id))(_ union _)
          params.log(s"Node: ${loc.name} Oldvalue: ${current(loc.id).mkString(env.variables)}" +
            s" Newinput: ${unionednew.mkString(env.variables)}")
          val newvalue = if (w.isEmpty) unionednew else w.get(current(loc.id), unionednew)
          params.log(s" Newvalue: ${newvalue.mkString(env.variables)}\n")
          newvalue
        }
      } while (current != next)

      params.log("Beginning descending chain\n")
      do {
        current = next
        next = for ((loc, n) <- locations zip narrowings) yield {
          val propnew = for (t <- loc.incoming) yield t.analyze(current(t.start.id))
          val unionednew = propnew.fold(initial(loc.id))(_ union _)
          params.log(s"Node: ${loc.name} Oldvalue: ${current(loc.id).mkString(env.variables)} " +
            s"Newinput: ${unionednew.mkString(env.variables)}")
          val newvalue = if (n.isEmpty) unionednew else n.get(current(loc.id), unionednew)
          params.log(s" Newvalue: ${
            newvalue.mkString(env.variables)
          }\n")
          newvalue
        }
      } while (current != next)
      locations.foreach {
        loc => ann(loc) = current(loc.id)
      }

    } else {
      val current = initial.toBuffer
      val workList = collection.mutable.Queue[Int]()

      params.log("Beginning ascending chain\n")
      workList ++= 0 until numlocs
      while (workList.nonEmpty) {
        val locid = workList.dequeue()
        val loc = locations(locid)
        val w = widenings(locid)
        val propnew = for (t <- loc.incoming) yield t.analyze(current(t.start.id))
        val unionednew = propnew.fold(initial(locid))(_ union _)
        params.log(s"Node: ${loc.name} Oldvalue: ${current(loc.id).mkString(env.variables)} " +
          s"Newinput: ${unionednew.mkString(env.variables)}")
        val newvalue = if (w.isEmpty) unionednew else w.get(current(locid), unionednew)
        params.log(s" Newvalue: ${newvalue.mkString(env.variables)}\n")
        if (newvalue != current(locid)) {
          current(locid) = newvalue
          for (t <- loc.outgoing) {
            if (!workList.contains(t.end.id)) workList.enqueue(t.end.id)
          }
        }
      }

      params.log("Beginning descending chain\n")
      workList ++= 0 until numlocs
      while (workList.nonEmpty) {
        val locid = workList.dequeue()
        val loc = locations(locid)
        val n = narrowings(locid)
        val propnew = for (t <- loc.incoming) yield t.analyze(current(t.start.id))
        val unionednew = propnew.fold(initial(locid))(_ union _)
        params.log(s"Node: ${loc.name} Oldvalue: ${current(loc.id).mkString(env.variables)} " +
          s"Newinput: ${unionednew.mkString(env.variables)}")
        val newvalue = if (n.isEmpty) unionednew else n.get(current(locid), unionednew)
        params.log(s" Newvalue: ${newvalue.mkString(env.variables)}\n")
        if (newvalue != current(locid)) {
          current(locid) = newvalue
          for (t <- loc.outgoing; if !workList.contains(t.end.id))
            workList.enqueue(t.end.id)
        }
      }
      locations.foreach {
        loc => ann(loc) = current(loc.id)
      }
    }
    ann
  }

  /**
    * Returns an EQS adapter given a numerical domain
    */
  def toEQS(dom: NumericalDomain): TargetAdapter[EQS[Location, dom.Property]] = new TargetAdapter[EQS[Location, dom.Property]] {
    val transformed = EQS(toEquationSystem(dom))

    def pullbackAnnotation[V](ann: Annotation[Location, V]): Annotation[Location, V] = ann
  }

  /**
    * Returns a string representing the annotation `ann` with correct variable names.
    */
  def mkString[U <: DimensionFiberedProperty[U]](ann: Annotation[ProgramPoint, U]): String = {
    (for ((loc, prop) <- ann) yield loc.name + " : " + prop.mkString(env.variables)).mkString(", \n")
  }

  /**
    * Returns a string representation of the LTS.
    */
  def mkString: String = locations.mkString("\n") + "\n" +
    (transitions map (_.mkString(env.variables))).mkString("\n") +
    "\n" + (regions map (_.mkString(env.variables))).mkString("\n")

  override def toString: String = name
}

object LTS {
  def apply(name: String, locations: IndexedSeq[Location], transitions: Seq[Transition],
            env: Environment, regions: Seq[Region] = Seq()): LTS =
    new LTS(name, locations, transitions, env, regions)
}
