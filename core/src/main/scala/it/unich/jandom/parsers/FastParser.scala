/**
  * Copyright 2013, 2016, 2018 Gianluca Amato
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

package it.unich.jandom.parsers

import it.unich.jandom.targets.NumericCondition._
import it.unich.jandom.targets.lts._
import it.unich.jandom.targets.{Environment, NumericAssignment, NumericCondition}

import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Reader

/**
  * Parser for transition systems as they appear in the Fast analyzer.
  *
  * @param env     initial environment.
  * @param postfix string to add at the end of the name field.
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */
class FastParser(env: Environment, postfix: String = "")
  extends JavaTokenParsers with NumericExpressionParser with NumericConditionParser {

  private val location_env = new mutable.HashMap[String, Location]

  // handle // as the start of a comment
  override val whiteSpace: Regex =
    """(\s|//.*\r?\n)+""".r

  override val mulExpr: Parser[Any] = "*" | guard(ident)

  protected val variable: Parser[Int] =
    ident ^^ {
      env(_)
    }

  private val var_declaration: Parser[Int] =
    ident ^^ env.addBinding

  private val var_declarations: Parser[List[Int]] =
    "var" ~> repsep(var_declaration, ",") <~ ";"

  private val state_declaration: Parser[Location] =
    ident ^^ { name =>
      val loc = Location(name, Seq())
      location_env += name -> loc
      loc
    }

  private val state_declarations: Parser[Seq[Location]] =
    "states" ~> repsep(state_declaration, ",") <~ ";"

  private val assignment: Parser[NumericAssignment] =
    (ident <~ "'" <~ "=") ~ numexpr ^^ {
      case v ~ e => NumericAssignment(env(v), e)
    }

  private val transition_declaration: Parser[Transition] =
    ("transition" ~> ident <~ ":=" <~ "{") ~
      ("from" ~> ":=" ~> ident <~ ";") ~
      ("to" ~> ":=" ~> ident <~ ";") ~
      ("guard" ~> ":=" ~> numcondition <~ ";") ~
      ("action" ~> ":=" ~> repsep(assignment, ",") <~ ";") <~
      "}" <~ ";" ^^ {
      case name ~ lstart ~ lend ~ guards ~ assignments =>
        Transition(name, location_env(lstart), location_env(lend), Seq(guards), assignments)
    }

  private val model: Parser[(String, Seq[Location], Seq[Transition])] = {
    ("model" ~> ident) ~ ("{" ~> var_declarations ~> state_declarations ~ rep(transition_declaration) <~ "}") ^^ {
      case name ~ (states ~ transitions) => (name, states, transitions)
    }
  }

  private val state_condition: Parser[Location] =
    "state" ~> "=" ~> ident ^^ location_env

  private val region_condition: Parser[(Option[Location], NumericCondition)] =
    (state_condition <~ "&&") ~ numcondition ^^ { case l ~ c => (Some(l), c) } |
      state_condition ^^ { l => (Some(l), TrueCond) } |
      numcondition ^^ { c => (None, c) }

  private val region: Parser[Region] =
    ("Region" ~> ident <~ ":=" <~ "{") ~ region_condition <~ "}" <~ ";" ^^ {
      case name ~ ((loc, cond)) => Region(name, loc, cond)
    }

  private val strategy: Parser[Seq[Region]] =
    "strategy" ~> ident ~> "{" ~> rep(region) <~ "}"

  val program: Parser[LTS] =
    model ~ strategy ^^ { case (name, states, transitions) ~ regions =>
      LTS(if (postfix.isEmpty) name else s"$name -- $postfix",
        states.toIndexedSeq, transitions, env, regions)
    }

  /**
    * The main parse function s.
    *
    * @param s the reader containing the FAST model
    * @return a ParseResult with the transition system parsed in the target LTS
    */
  def parse(s: Reader[Char]): ParseResult[LTS] = parseAll(program, s)

  /**
    * The main parse function s.
    *
    * @param s the reader containing the FAST model
    * @return a ParseResult with the transition system parsed in the target LTS
    */
  def parse(s: java.io.Reader): ParseResult[LTS] = parseAll(program, s)

  /**
    * The main parse function s.
    *
    * @param s the char sequence containing the FAST model
    * @return a ParseResult with the transition system parsed in the target LTS
    */
  def parse(s: CharSequence): ParseResult[LTS] = parseAll(program, s)
}

/**
  * The companion  object for FastParser. It only contains a factory method.
  *
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */
object FastParser {
  /**
    * Create a parser for Fast transition systems with a given environment and postfix string.
    *
    * @param env     an initial environment. It is optional and defaults to the empty environment.
    * @param postfix an optional string to add at the end of the name field.
    */
  def apply(env: Environment = new Environment(), postfix: String = "") = new FastParser(env, postfix)
}
