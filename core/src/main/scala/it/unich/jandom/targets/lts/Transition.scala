/**
  * Copyright 2013, 2014, 2018 Gianluca Amato <gianluca.amato@unich.it>
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

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.{NumericAssignmentMultiple, NumericCondition}

/**
  * The class for transitions.
  *
  * @param name        the name of the transition
  * @param start       the source location of the transition
  * @param end         the target location of the transition
  * @param guard       the conditions which should hold for the transition to be active
  * @param assignments the assignments to apply when the transition is selected
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */
class Transition(val name: String, val start: Location, val end: Location, val guard: Seq[NumericCondition],
                 val assignments: NumericAssignmentMultiple) {

  // wires the location's incoming and outgoing pointer to this transition
  end.incoming +:= this
  start.outgoing +:= this

  /**
    * Returns true if `that` is syntactically equal to `this`.
    */
  def syntacticallyEquals(that: Transition): Boolean =
    name == that.name &&
      start.syntacticallyEquals(that.start) &&
      end.syntacticallyEquals(that.end) &&
      guard == that.guard &&
      assignments == that.assignments

  /**
    * Returns a string representation of a transition with given variable names.
    */
  def mkString(vars: Seq[String]): String = {
    "transition " + name + " " + start.name + " -> " + end.name + " with Guard( " +
      (guard map (_.mkString(vars))).mkString(", ") + " )\n" +
      assignments.mkString(vars).mkString(start = "  ", sep = "\n  ", end = "") + ";"
  }

  /**
    * Returns a string representation of a transition with synthetic variable names.
    */
  def mkString: String = mkString(Stream.from(0).map("v" + _))

  /**
    * Returns the result of executing the transition.
    *
    * @param input the input abstract property
    * @tparam Property the type of the abstract property
    * @return the resulting property
    */
  def analyze[Property <: NumericalProperty[Property]](input: Property): Property = {
    val filtered = (input /: guard) { (current, cond) => cond.analyze(current) }
    assignments.analyze(filtered)
  }

  override def toString: String = name

}

object Transition {
  def apply(name: String, start: Location, end: Location, guard: Seq[NumericCondition],
            assignments: NumericAssignmentMultiple): Transition = new Transition(name, start, end, guard, assignments)
}
