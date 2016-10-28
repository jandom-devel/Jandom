/**
 * Copyright 2013, 2016 Gianluca Amato
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

package it.unich.jandom.parsers

import scala.collection.mutable.HashMap
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Reader
import it.unich.jandom.targets.Environment
import it.unich.jandom.targets.NumericAssignment
import it.unich.jandom.targets.NumericCondition
import it.unich.jandom.targets.NumericCondition._
import it.unich.jandom.targets.NumericExpression._
import it.unich.jandom.targets.lts._
import it.unich.jandom.targets.NumericAssignmentMultiple

/**
 * Parser for transition systems as they appear in the Fast analyzer.
 * @author Gianluca Amato <gamato@unich.it>
 */
class FastParser extends JavaTokenParsers with NumericExpressionParser with NumericConditionParser {
  private val env: Environment = new Environment()

  private val location_env = new HashMap[String, Location]

  // handle // as the start of a comment
  override val whiteSpace = """(\s|//.*\r?\n)+""".r

  override val mulExpr: Parser[Any] = "*" | guard(ident)

  protected val variable: Parser[Int] =
    ident ^^ { env(_) }

  private val var_declaration: Parser[Int] =
    ident ^^ { case v => env.addBinding(v) }

  private val var_declarations: Parser[List[Int]] =
    "var" ~> repsep(var_declaration, ",") <~ ";"

  private val state_declaration: Parser[Location] =
    ident ^^ {
      case name =>
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
        case name ~ lstart ~ lend ~ guards ~ assignments => {
          Transition(name, location_env(lstart), location_env(lend), Seq(guards), assignments)
        }
      }

  private val model: Parser[(String, Seq[Location], Seq[Transition])] = {
    ("model" ~> ident) ~ ("{" ~> var_declarations ~> state_declarations ~ rep(transition_declaration) <~ "}") ^^ {
      case name ~ (states ~ transitions) => (name, states, transitions)
    }
  }

  private val state_condition: Parser[Location] =
    "state" ~> "=" ~> ident ^^ { case start => location_env(start) }

  private val region_condition: Parser[(Option[Location], NumericCondition)] =
    (state_condition <~ "&&") ~ numcondition ^^ { case l ~ c => (Some(l), c) } |
      state_condition ^^ { case l => (Some(l), TrueCond) } |
      numcondition ^^ { case c => (None, c) }

  private val region: Parser[Region] =
    ("Region" ~> ident <~ ":=" <~ "{") ~ region_condition <~ "}" <~ ";" ^^ {
      case name ~ ((loc, cond)) => Region(name, loc, cond)
    }

  private val strategy: Parser[Seq[Region]] =
    "strategy" ~> ident ~> "{" ~> rep(region) <~ "}"

  val program: Parser[LTS] =
    model ~ strategy ^^ { case (name, states, transitions) ~ regions =>  new LTS(name, states.toIndexedSeq, transitions, env, regions) }

  /**
   * The main parse function s.
   * @param s the reader containing the FAST model
   * @return a ParseResult with the transition system parsed in the target LTS
   */
  def parse(s: Reader[Char]) = parseAll(program, s)

  /**
   * The main parse function s.
   * @param s the reader containing the FAST model
   * @return a ParseResult with the transition system parsed in the target LTS
   */
  def parse(s: java.io.Reader) = parseAll(program, s)

  /**
   * The main parse function s.
   * @param in the char sequence containing the FAST model
   * @return a ParseResult with the transition system parsed in the target LTS
   */
  def parse(s: CharSequence) = parseAll(program, s)
}

/**
 * The companion  object for FastParser. It only contains a factory method.
 * @author Gianluca Amato <gamato@unich.it>
 */
object FastParser {
  /**
   * Returns a FastParser.
   */
  def apply() = new FastParser()
}
