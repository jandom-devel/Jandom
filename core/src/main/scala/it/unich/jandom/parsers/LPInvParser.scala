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

package it.unich.jandom.parsers

import scala.collection.mutable.HashMap
import scala.util.parsing.combinator.JavaTokenParsers

import it.unich.jandom.targets.Environment
import it.unich.jandom.targets.LinearAssignment
import it.unich.jandom.targets.linearcondition.AtomicCond
import it.unich.jandom.targets.lts.LTS
import it.unich.jandom.targets.lts.Location
import it.unich.jandom.targets.lts.Transition

/**
  * Parser for transition systems as they appear in the [[http://www.cs.colorado.edu/~srirams/Software/lpinv.html LPInv]]
  * invariant generator by Sriram Sankaranarayanan. It generates an LTS (Linear Transition System) target.
  * It actually parser a super-set of the LPInv transitions systems, since it possible to specify complex
  * conditions with &&, || and ! in the locations.
  * @author Gianluca Amato <gamato@unich.it>
  */
class LPInvParser(val env: Environment) extends JavaTokenParsers with LinearFormParser with LinearConditionParser {
  private val location_env = new HashMap[String, Location]

  override val whiteSpace = """(\s|#.*\r?\n)+""".r // handle # as the start of a comment

  val variable: Parser[Int] =
    ident ^^ { env(_) }

  private val var_declaration: Parser[Any] =
    ident ^^ { case v => env.addBinding(v) }

  private val declarations: Parser[Any] =
    literal("var") ~> repsep(var_declaration, ",") <~ ";"

  // the template part is ignored for now	
  private val template: Parser[Any] =
    literal("Template") ~> "(" ~> repsep("""[a-zA-Z$+-]*""".r, ",") <~ ")" <~ ";"

  private val location: Parser[Location] =
    (literal("location") ~> ident <~ literal("with") <~ "(") ~
      rep(condition) <~
      ")" <~ ";" ^^ {
        case name ~ condition => {
          val loc = Location(name, condition)
          location_env += name -> loc
          loc
        }
      }

  private val assignment: Parser[LinearAssignment[Int]] =
    (ident <~ ":=") ~ linform ^^ {
      case v ~ lf => LinearAssignment(env.getBindingOrAdd(v), lf)
    }

  private val transition: Parser[Transition] =
    (literal("transition") ~> ident) ~ (ident <~ "->") ~ (ident <~ literal("with") <~ literal("Guard")) ~
      ("(" ~> rep(condition) <~ ")") ~
      rep(assignment) <~ ";" ^^ {
        case name ~ lstart ~ lend ~ guards ~ assignments => {
          Transition(name, location_env(lstart), location_env(lend), guards, assignments)
        }
      }

  private val prog: Parser[LTS] =
    declarations ~> opt(template) ~> rep(location) ~ rep(transition) <~ literal("end") ^^ {
      case ls ~ ts => LTS(ls.toIndexedSeq, ts, env)
    }

  /**
    * The parse function.
    * @param s the string containing the linear transition system
    * @return a ParseResult with the transition system parsed in the target LTS
    */
  def parseProgram(s: String) = parseAll(prog, s)
}

/** Factory for [[it.unich.jandom.parsers.LPInvParser]] instances. */
object LPInvParser {
  /** 
   * Create a parser for LPInv transition systems with a given environment.
   * @param env the environment. It is optional and defaults to the empty environment.
   */
  def apply(env: Environment = new Environment()) = new LPInvParser(env)
}

