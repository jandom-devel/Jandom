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
 * (c) 2011 Gianluca Amato
 */

package it.unich.sci.jandom
package parsers

import targets.{ Environment, LinearForm }
import targets.slil._
import targets.linearcondition._
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Parser for Random programs.
 * @param env the environment for the parser
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
class RandomParser(val env: Environment) extends JavaTokenParsers with LinearExpressionParser with LinearConditionParser {

  override val whiteSpace = """(\s|#.*\r?\n)+""".r // handle # as the start of a comment

  override def stringLiteral = "\"[^\"]*\"".r // allow CR in string literals

  override val ident = not(literal("function")) ~> """[a-zA-Z._][\w.]*""".r // allow . in identifiers

  val variable: Parser[Int] = ident ^^ { env.getBindingOrAdd(_) }

  private val stmt: Parser[SLILStmt] =
    "tag" ~> "(" ~> wholeNumber <~ ")" ^^ { case s => TagStmt(s.toInt) } |
      "assume" ~> "(" ~> condition <~ ")" ^^ { AssumeStmt(_) } |
      ("if" ~> "(" ~> condition <~ ")") ~ stmt ~ opt("else" ~> stmt) ^^ {
        case c ~ s1 ~ Some(s2) => IfStmt(c, s1, s2)
        case c ~ s1 ~ None => IfStmt(c, s1, NopStmt)
      } |
      ("while" ~> "(" ~> condition <~ ")") ~ stmt ^^ {
        case c ~ s => WhileStmt(c, s)
      } |
      "{" ~> repsep(stmt, opt(";")) <~ "}" ^^ { CompoundStmt(_) } |
      ident ~ ("=" | "<-") ~ expr ^^ { case v ~ _ ~ lf => AssignStmt(env.getBindingOrAdd(v), lf) }

  private val prog: Parser[SLILProgram] =
    (opt(ident) ~> ("=" | "<-") ~> "function" ~> "(" ~> repsep(variable, ",") <~ ")") ~ stmt ^^ {
      case vars ~ stmt => SLILProgram(env, vars, stmt)
    }

  private val skip: Parser[String] = """(.|[\r\n])*""".r // skip until the end of the file

  /**
   * This parser a program, eventually preceded by an if statement used to embed an INTERPROC version
   * of the program. We are assuming that the first function definition in the file is the function
   * we want to analyze. Other definitions are input for the trace analyzer in Random which is not
   * implemented yet.
   */
  private val progWithCases: Parser[SLILProgram] =
    opt("if" ~ "(" ~ "FALSE" ~ ")" ~ stringLiteral) ~> prog <~ skip

  /**
   * The parse function
   * @param s the string containing the Random Program
   * @return a ParseResult with the program parsed in the target SLILProgram
   */
  def parseProgram(s: String) = parseAll(progWithCases, s)
}

/** Factory for [[it.unich.sci.jandom.RandomParser]] instances. */
object RandomParser {
  /**
   * Create a parser for Random programs with a given environment.
   * @param env the environment. It is optional and defaults to the empty environment.
   */
  def apply(env: Environment = new Environment()) = new RandomParser(env)
}
