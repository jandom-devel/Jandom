/**
  * Copyright 2013, 2016, 2018 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.targets.slil

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.parameters._
import it.unich.jandom.targets.{Annotation, Environment, NumericCondition, lts}
import it.unich.jandom.ui.output.OutputBuilder

/**
  * The class for a while statement. Each while statement has a corresponding program
  * point which corresponds to the head of the loop, before the condition is tested.
  *
  * @param condition the guard of the statement
  * @param body      the body of the statement
  */
class WhileStmt(val condition: NumericCondition, val body: SLILStmt) extends SLILStmt {

  import AnalysisPhase._

  /**
    * This variable keeps the last value for the invariant computed during the analysis.
    */
  private var lastInvariant: NumericalProperty[_] = _

  /**
    * This variable keeps the last result for the analysis of the body of the While statement.
    */
  private var lastBodyResult: NumericalProperty[_] = _

  def analyzeStmt(params: Parameters)(input: params.Property, phase: AnalysisPhase, ann: Annotation[ProgramPoint, params.Property]): params.Property = {

    import NarrowingStrategy._
    import WideningScope._

    // Increase nesting level since we are entering a loop
    params.nestingLevel += 1

    // Determines widening/narrowing operators to use
    val widening = params.widening((this, Symbol("head")))
    val narrowing = params.narrowing((this, Symbol("head")))

    // Determines initial values for the analysis, depending on the calling phase
    var (bodyResult, invariant) =
      if (lastBodyResult != null && phase != AscendingRestart)
        (lastBodyResult.asInstanceOf[params.Property], lastInvariant.asInstanceOf[params.Property])
      else
        (input.bottom, input.bottom)

    // Keep the current phase in the variable currentPhase, and initialize
    // with the input parameter
    var currentPhase = phase

    // Declare a variable for the loop
    var newinvariant = invariant

    if (currentPhase == Ascending || currentPhase == AscendingRestart) {
      // Debug
      params.log("Beginning Ascending Chain\n")
      params.log(s"Starting Invariant: $invariant\n")
      params.log(s"Input: $input\n")

      // Initialization phase: compute the effect of entering the while node from the
      // outer cycle.
      params.wideningScope match {
        case Random => newinvariant = invariant union input
        case Output => newinvariant = invariant widening (input union bodyResult)
        case BackEdges => newinvariant = bodyResult union input
      }

      // Debug
      params.log(s"Entering Invariant: $newinvariant\n")

      do {
        invariant = newinvariant
        params.wideningScope match {
          case Random =>
            bodyResult = body.analyzeStmt(params)(condition.analyze(invariant), currentPhase, ann)
            newinvariant = widening(invariant, bodyResult)
          case BackEdges =>
            bodyResult = bodyResult widening body.analyzeStmt(params)(condition.analyze(input union newinvariant), currentPhase, ann)
            newinvariant = bodyResult union input
          case Output =>
            bodyResult = body.analyzeStmt(params)(condition.analyze(newinvariant), currentPhase, ann)
            newinvariant = widening(invariant, input union bodyResult)
        }
        // If we were in AscendingRestart phase, move to Ascending phase
        currentPhase = Ascending

        // Debug
        params.log(s"Body Result: $bodyResult\n")
        params.log(s"Invariant: $newinvariant\n")

      } while (newinvariant > invariant)

      // Debug
      params.log(s"Final ascending invariant: $invariant\n")

      // If the strategy wants intertwined ascending and descending chains,
      // move phase to Descending
      if (params.narrowingStrategy == Restart || params.narrowingStrategy == Continue)
        currentPhase = Descending
    }

    if (currentPhase == Descending) {
      // Debug
      params.log("Beginning Descending Chain\n")
      params.log(s"Starting Invariant: $invariant\n")
      params.log(s"Starting Body Result: $bodyResult\n")
      params.log(s"Input: $input\n")

      // For narrowing, we only consider output scope
      newinvariant = narrowing(invariant, input union bodyResult)

      // Debug
      params.log(s"Entering Invariant: $newinvariant\n")

      // Determines the phase for the inner loops
      val newphase = if (params.narrowingStrategy == Restart) AscendingRestart else Descending
      do {
        invariant = newinvariant

        bodyResult = body.analyzeStmt(params)(condition.analyze(invariant), newphase, ann)
        newinvariant = invariant narrowing (invariant intersection (input union bodyResult))

        // Debug
        params.log(s"Body Result: $bodyResult\n")
        params.log(s"Invariant: $newinvariant\n")
      } while (newinvariant < invariant)

      params.log(s"Final descending invariant: $newinvariant\n")
    }

    // Save current values for later iterations of the loop
    lastInvariant = invariant
    lastBodyResult = bodyResult

    // Annotate results
    ann((this, Symbol("head"))) = invariant
    if (params.allPPResult) {
      ann((this, Symbol("bodyStart"))) = condition.analyze(invariant)
      ann((this, Symbol("bodyEnd"))) = bodyResult
    }

    // Exit from this loop, hence decrement nesting level
    params.nestingLevel -= 1

    condition.opposite.analyze(invariant)
  }

  def outputAnnotation[T <: NumericalProperty[_]](ann: Annotation[ProgramPoint, T], ob: OutputBuilder, env: Environment): Unit = {
    ob ++= s"while (${condition.mkString(env.names)})"
    for (p <- ann.get((this, Symbol("head")))) (ob ++= " ").annotate(p.mkString(env.variables))
    ob ++= " {"
    ob.newline(ob.IndentBehaviour.Increase)
    for (p <- ann.get((this, Symbol("bodyStart")))) ob.annotate(p.mkString(env.variables)).newline()
    body.outputAnnotation(ann, ob, env)
    for (p <- ann.get((this, Symbol("bodyEnd")))) ob.newline().annotate(p.mkString(env.variables))
    ob.newline(ob.IndentBehaviour.Decrease)
    ob ++= "}"
  }

  def syntacticallyEquals(that: SLILStmt): Boolean = that match {
    case that: WhileStmt =>
      condition == that.condition && body.syntacticallyEquals(that.body)
    case _ => false
  }

  val numvars: Int = condition.dimension max body.numvars

  def toLTS(prev: lts.Location, next: lts.Location): (Map[ProgramPoint, lts.Location], Seq[lts.Transition]) = {
    val headpp = lts.Location((this, Symbol("head")).toString, Seq())
    val truepp = lts.Location((this, Symbol("bodyStart")).toString, Seq())
    val tailpp = lts.Location((this, Symbol("bodyEnd")).toString, Seq())
    val tenter = lts.Transition((this, Symbol("enter")).toString, prev, headpp, Seq(), Seq.empty)
    val texit = lts.Transition((this, Symbol("exit")).toString, tailpp, headpp, Seq(), Seq.empty)
    val ttrue = lts.Transition((this, Symbol("true")).toString, headpp, truepp, Seq(condition), Seq.empty)
    val tfalse = lts.Transition((this, Symbol("false")).toString, headpp, next, Seq(condition.opposite), Seq.empty)
    val tt1 = body.toLTS(truepp, tailpp)
    (tt1._1 ++ Seq((this, Symbol("head")) -> headpp, (this, Symbol("bodyStart")) -> truepp, (this, Symbol("bodyEnd")) -> tailpp),
      tt1._2 :+ tenter :+ ttrue :+ tfalse :+ texit)
  }

  // Optimized alternative definition of wire.
  /*
  def toLTS(prev: lts.Location, next: lts.Location): (Map[ProgramPoint, lts.Location], Seq[lts.Transition]) = {
    val pp = lts.Location((this, Symbol("head")).toString, Seq())
    val t1 = lts.Transition((this, 'enter).toString, prev, pp, Seq(condition), Seq.empty)
    val t2 = lts.Transition((this, 'exit).toString, prev, next, Seq(condition.opposite), Seq.empty)
    val tt1 = body.toLTS(pp, prev)
    (tt1._1 updated((this, Symbol("head")), pp), tt1._2 :+ t1 :+ t2)
  }
  */

  override def toString: String = s"while@$hashCode ($condition)"
}

object WhileStmt {
  def apply(condition: NumericCondition, body: SLILStmt): WhileStmt = new WhileStmt(condition, body)
}
