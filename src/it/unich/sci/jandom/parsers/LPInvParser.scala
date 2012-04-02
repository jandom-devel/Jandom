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
package it.unich.sci.jandom.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.mutable.HashMap
import it.unich.sci.jandom.targets.LinearForm
import it.unich.sci.jandom.targets.Environment
import it.unich.sci.jandom.targets.linearcondition._
import it.unich.sci.jandom.targets.lts._

/**
 * Parser for transition systems as appear in the [[http://www.cs.colorado.edu/~srirams/Software/lpinv.html LPInv]]
 * invariant generator by Sriram Sankaranarayanan. It generates an LTS (Linear Transition System) target.
 * It actually parser a super-set of the LPInv transitions systems, since it possible to specify complex
 * conditions with &&, || and ! in the locations.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
object LPInvParser extends JavaTokenParsers with LinearExpressionParser with LinearConditionParser {
  val env = Environment()
  var current_loc = 0
  private val location_env = new HashMap[String, Location]

  override val whiteSpace = """(\s|#.*\r?\n)+""".r // handle # as the start of a comment

  val variable: Parser[Int] =
    ident ^^ { env(_) }
  
  override val operator_alias: Parser[String] = 
    "=" ^^ { _ => "==" }

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
          val loc = Location(name, current_loc, condition)
          current_loc += 1
          location_env += name -> loc
          loc
        }
      }

  private val assignment: Parser[Assignment[Int]] =
    (ident <~ ":=") ~ expr ^^ {
      case v ~ lf => Assignment(env.getBindingOrAdd(v), lf)
    }

  private val transition: Parser[Transition] =
    (literal("transition") ~> ident) ~ (ident <~ "->") ~ (ident <~ literal("with") <~ literal("Guard")) ~
      ("(" ~> rep(condition) <~ ")") ~
      rep(assignment) <~ ";" ^^ {
        case name ~ lstart ~ lend ~ guards ~ assignments => {
          location_env(lend) += Transition(name, location_env(lstart), location_env(lend), guards, assignments)          
        }         
      }

  private val prog = 
    declarations ~> opt(template) ~> rep(location) ~ rep(transition) <~ literal("end") ^^ {
      case ls ~ ts => LTS(ls, ts, env)
    }

  def parseProgram(s: String) = parseAll(prog, s)
}
