/**
  * Copyright 2013, 2014, 2018 Gianluca Amato <gamato@unich.it>
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

import it.unich.jandom.targets.{Environment, NumericCondition}
import it.unich.jandom.targets.NumericCondition._
import it.unich.jandom.targets.slil._

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Reader


/**
  * Parser for Random programs.
  *
  * @param env the initial environment.
  * @author Gianluca Amato <gianluca.amato@unich.it>
  */
class RandomParser(val env: Environment) extends JavaTokenParsers with NumericExpressionParser with NumericConditionParser {

  override val whiteSpace: Regex = """(\s|#.*\r?\n)+""".r // handle # as the start of a comment

  override def stringLiteral: Parser[String] = "\"[^\"]*\"".r // allow CR in string literals

  override val ident: Parser[String] = not(literal("function")) ~> """[a-zA-Z._][\w.]*""".r // allow . in identifiers

  override val divExpr: Parser[String] = "/" | "%/%"

  private val variableFollow: Parser[Unit] = not("""[\[(]""".r) // follow a valid variable

  val variable: Parser[Int] = ident <~ variableFollow ^^ env.getBindingOrAdd

  private val atom: Parser[String] =
    (wholeNumber |
      funCall |
      arrayAccess |
      "(" ~ expr ~ ")" |
      ident) ^^ {
      _.toString
    }

  private val expr: Parser[String] =
    (atom <~ "+" <~ expr |
      atom <~ "-" <~ expr |
      atom <~ "*" <~ expr |
      atom <~ "/" <~ expr |
      atom <~ "%/% " <~ expr |
      atom) ^^ {
      _.toString
    }

  private val funCall = ident ~ "(" ~ repsep(expr, ",") ~ ")" ^^ (_.toString)

  private val arrayAccess = ident ~ "[" ~ repsep(expr, ",") ~ "]" ^^ (_.toString)

  private val general_atomic_condition: Parser[String] =
    ("TRUE" |
      "FALSE" |
      "brandom" |
      expr ~ comparison ~ expr) ^^ {
      _.toString
    }

  private val general_condition: Parser[NumericCondition] =
    numcondition |
      (general_atomic_condition |
        general_atomic_condition ~ "&&" ~ general_condition |
        general_atomic_condition ~ "||" ~ general_condition |
        "!" ~ general_condition |
        "(" ~ general_condition ~ ")") ^^ { _ => BRandomCond }

  private val stmt: Parser[SLILStmt] =
    "tag" ~> "(" ~> wholeNumber <~ ")" <~ opt(";") ^^ { s => TagStmt(s.toInt) } |
      ".tracetag" ~ "(" ~ wholeNumber ~ ")" <~ opt(";") ^^ { _ => NopStmt() } |
      "assume" ~> "(" ~> numcondition <~ ")" <~ opt(";") ^^ AssumeStmt.apply |
      ("if" ~> "(" ~> general_condition <~ ")") ~ stmt ~ opt("else" ~> stmt) ^^ {
        case c ~ s1 ~ Some(s2) => IfStmt(c, s1, s2)
        case c ~ s1 ~ None => IfStmt(c, s1, NopStmt())
      } |
      ("while" ~> "(" ~> general_condition <~ ")") ~ stmt ^^ {
        case c ~ s => WhileStmt(c, s)
      } |
      ("return" ~ "(" ~ expr ~ ")") ^^ { _ => NopStmt() } |
      "{" ~> rep(stmt) <~ "}" ^^ {
        CompoundStmt(_: _*)
      } |
      ident ~ ("=" | "<-") ~ numexpr <~ opt(";") ^^ { case v ~ _ ~ e => AssignStmt(env.getBindingOrAdd(v), e) } |
      expr <~ ("=" | "<-") <~ expr <~ opt(";") ^^ { _ => NopStmt() }

  private val prog: Parser[SLILProgram] =
    (opt(ident ~> ("=" | "<-")) ~> "function" ~> "(" ~> repsep(variable, ",") <~ ")") ~ stmt ^^ {
      case vars ~ statement => SLILProgram(env, vars, statement)
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
    * The parse function.
    *
    * @param s the string containing the Random Program
    * @return a ParseResult with the program parsed in the target SLILProgram
    */
  def parse(s: CharSequence): ParseResult[SLILProgram] = parseAll(progWithCases, s)

  /**
    * The parse function.
    *
    * @param s the string containing the Random Program
    * @return a ParseResult with the program parsed in the target SLILProgram
    */
  def parse(s: java.io.Reader): ParseResult[SLILProgram] = parseAll(progWithCases, s)

  /**
    * The parse function.
    *
    * @param s the string containing the Random Program
    * @return a ParseResult with the program parsed in the target SLILProgram
    */
  def parse(s: Reader[Char]): ParseResult[SLILProgram] = parseAll(progWithCases, s)
}

/**
  * Companion object for [[it.unich.jandom.parsers.RandomParser]].
  */
object RandomParser {
  /**
    * Create a parser for Random programs with a given environment.
    *
    * @param env the initial environment. It is optional and defaults to the empty environment.
    */
  def apply(env: Environment = new Environment()) = new RandomParser(env)
}
