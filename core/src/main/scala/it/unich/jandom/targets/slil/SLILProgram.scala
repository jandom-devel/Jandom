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

import it.unich.jandom.domains.numerical.{NumericalDomain, NumericalProperty}
import it.unich.jandom.targets.eqs.EQS
import it.unich.jandom.targets.lts._
import it.unich.jandom.targets.parameters.NarrowingStrategy
import it.unich.jandom.targets.{Annotation, Environment, NumericCondition, lts}
import it.unich.jandom.ui.output.{OutputBuilder, TextOutputBuilder}

/**
  * The target for a simple imperative language, similar to the one analyzed
  * by Random. Each program is essentially a function with some input variables and
  * a body, with a single scope which extends to the entire body.
  *
  * @param env       the environment for the program
  * @param inputVars the input variables
  * @param stmt      the body of the program
  * @author Gianluca Amato <gamato@unich.it>
  */
class SLILProgram(val env: Environment, val inputVars: Seq[Int], val stmt: SLILStmt) extends SLILTarget {

  import AnalysisPhase._

  /**
    * A method for sending the program with related annotations to an output builder.
    *
    * @param ann the annotation to print together with the program
    * @param ob  the output builder to use for printing the result
    */
  def outputAnnotation[T <: NumericalProperty[_]](ann: Annotation[ProgramPoint, T], ob: OutputBuilder): Unit = {
    ob ++= "function ("
    ob ++= (inputVars map { v: Int => env(v) }).mkString(",")
    ob ++= ") {"
    ob.newline(ob.IndentBehaviour.Increase)
    for (p <- ann.get((this, 'start)))
      ob.annotate(p.mkString(env.variables)).newline()
    stmt.outputAnnotation(ann, ob, env)
    for (p <- ann.get((this, 'end)))
      ob.newline().annotate(p.mkString(env.variables))
    ob.newline(ob.IndentBehaviour.Decrease)
    ob ++= "}"
  }

  /**
    * A method for printing the program the with related annotations.
    *
    * @param ann the annotation to print together with the program
    * @return the string representation of the program
    */
  def mkString[T <: NumericalProperty[_]](ann: Annotation[ProgramPoint, T]): String = {
    val ob = TextOutputBuilder()
    outputAnnotation(ann, ob)
    ob.toString
  }

  def analyze(params: Parameters): Annotation[ProgramPoint, params.Property] = {
    val input = params.domain.top(env.size)
    val ann = getAnnotation[params.Property]
    val output = params.narrowingStrategy match {
      case NarrowingStrategy.Separate =>
        stmt.analyzeStmt(params)(input, Ascending, ann)
        stmt.analyzeStmt(params)(input, Descending, ann)
      case _ =>
        stmt.analyzeStmt(params)(input, Ascending, ann)
    }
    if (params.allPPResult) {
      ann((this, 'start)) = input
      ann((this, 'end)) = output
    }
    ann.asInstanceOf[Annotation[ProgramPoint, params.Property]]
  }

  val lastPP: Option[ProgramPoint] = stmt.lastPP

  /**
    * Returns an LTS adapter for this program.
    */
  lazy val toLTS: TargetAdapter[lts.LTS] = {
    val startPP = lts.Location("start", Seq())
    val endPP = lts.Location("end", Seq())
    val (ppmapTmp, transitions) = stmt.toLTS(startPP, endPP)
    val ppmap = ppmapTmp ++ Seq((this, 'start) -> startPP, (this, 'end) -> endPP)
    val locations: IndexedSeq[Location] = ppmap.values.toIndexedSeq
    new TargetAdapter[lts.LTS] {
      val transformed: lts.LTS = lts.LTS("slil", locations, transitions, env,
        Seq(lts.Region("init", Some(startPP), NumericCondition.TrueCond)))

      def pullbackAnnotation[V](ann: Annotation[lts.Location, V]): Annotation[ProgramPoint, V] = {
        val myAnn = getAnnotation[V]
        for (u <- ppmap.keys) myAnn(u) = ann(ppmap(u))
        myAnn
      }
    }
  }

  /**
    * Returns true if `that` is syntactically equal to `this`.
    */
  def syntacticallyEquals(that: SLILProgram): Boolean =
    env == that.env && inputVars == that.inputVars && stmt.syntacticallyEquals(that.stmt)

  /**
    * Returns an EQS adapter for this program (given a numerical domain).
    */
  def toEQS(dom: NumericalDomain): TargetAdapter[EQS[lts.Location, dom.Property]] =
    new TargetAdapter[EQS[lts.Location, dom.Property]] {
      val transformed = EQS(toLTS.transformed.toEquationSystem(dom))

      def pullbackAnnotation[V](ann: Annotation[lts.Location, V]): Annotation[ProgramPoint, V] =
        toLTS.pullbackAnnotation(ann)
    }
}

object SLILProgram {
  def apply(env: Environment, inputVars: Seq[Int], stmt: SLILStmt): SLILProgram =
    new SLILProgram(env, inputVars, stmt)
}
