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
import it.unich.sci.jandom.targets.LTS._

/**
 * Parser for transition systems as appear in the [[http://www.cs.colorado.edu/~srirams/Software/lpinv.html LPInv]]
 * invariant generator by Sriram Sankaranarayanan. It generates an LTS (Linear Transition System) target.
 * It actually parser a super-set of the LPInv transitions systems, since it possible to specify complex
 * conditions with &&, || and ! in the locations.
 * @author Gianluca Amato <amato@sci.unich.it>
 */
object LPInvParser extends JavaTokenParsers {
  private val env = Environment()

  private val location_env = new HashMap[String, Location]

  override val whiteSpace = """(\s|#.*\r?\n)+""".r // handle # as the start of a comment

  private val variable: Parser[Int] =
    ident ^^ { env(_) }

  private val term: Parser[LinearForm[Int]] =
    (opt(wholeNumber <~ "*") ~ variable) ^^ {
      case Some(coeff) ~ v => LinearForm.fromCoefficientVar[Int](coeff.toInt, v + 1, env)
      case None ~ v => LinearForm.fromVar[Int](v + 1, env)
    } |
      wholeNumber ^^ { case coeff => LinearForm.fromCoefficient(coeff.toInt, env) }

  private val term_with_operator: Parser[LinearForm[Int]] =
    "+" ~> term |
      "-" ~> term ^^ { lf => -lf }

  private val expr: Parser[LinearForm[Int]] =
    term ~ rep(term_with_operator) ^^ {
      case lf1 ~ lfarr => (lf1 /: lfarr) { (lfa, lfb) => lfa + lfb }
    }

  private val comparison: Parser[AtomicCond.ComparisonOperators.Value] =
    ("=" | "==" | "<=" | ">=" | "!=" | "<" | ">") ^^ {
      case "=" => AtomicCond.ComparisonOperators.withName("==")
      case s => AtomicCond.ComparisonOperators.withName(s)
    } |
      failure("invalid comparison operator")

  private val atomic_condition: Parser[LinearCond] =
    "FALSE" ^^ { s => FalseCond } |
      "TRUE" ^^ { s => TrueCond } |
      "brandom" ~ "(" ~ ")" ^^ { s => BRandomCond } |
      expr ~ comparison ~ expr ^^ { case lf1 ~ op ~ lf2 => AtomicCond(lf1 - lf2, op) }

  private val condition: Parser[LinearCond] =
    atomic_condition |
      atomic_condition ~ "&&" ~ condition ^^ { case c1 ~ _ ~ c2 => AndCond(c1, c2) } |
      atomic_condition ~ "||" ~ condition ^^ { case c1 ~ _ ~ c2 => OrCond(c1, c2) } |
      "!" ~> condition ^^ { case c => NotCond(c) }

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
        case id ~ condition => {
          val loc = Location(id, condition)
          location_env += id -> loc
          loc
        }
      }

  private val assignment: Parser[Assignment[Int]] =
    (ident <~ ":=") ~ expr ^^ {
      case v ~ lf => Assignment(env.getBindingOrAdd(v), lf)
    }

  private def transition: Parser[Transition] =
    (literal("transition") ~> ident) ~ (ident <~ "->") ~ (ident <~ literal("with") <~ literal("Guard")) ~
      ("(" ~> rep(condition) <~ ")") ~
      rep(assignment) <~ ";" ^^ {
        case name ~ lstart ~ lend ~ guards ~ assignments =>
          Transition(name, location_env(lstart), location_env(lend), guards, assignments)
      }

  private def prog = 
    declarations ~> opt(template) ~> rep(location) ~ rep(transition) <~ literal("end") ^^ {
      case ls ~ ts => LTS(ls, ts)
    }

  def parseProgram(s: String) = parseAll(prog, s)
}
