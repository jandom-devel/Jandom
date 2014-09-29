package it.unich.jandom.targets

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.domains.numerical.LinearForm

/**
 * A class for parallel assignments.
 * @param as the sequence of assignment which compose the multiple assignment
 */
case class NumericAssignmentMultiple(val as: Seq[NumericAssignment]) {
  require(!NumericAssignmentMultiple.containsDups(as))

  /**
   * This methods takes an input property and returns the result of computing the multiple assignment.
   */
  def analyze[Property <: NumericalProperty[Property]](input: Property): Property = {

    if (as.length == 0)
      input
    else if (as.length == 1)
      as.head.analyze(input)
    else {
      var current: Property = input
      // evaluate all expression which makes the multiple assignment, without actually assigning
      for (a <- as) {
        current = a.exp.analyze(current)
      }
      // assign all the new expressions (may be replaced with map)
      for ((a, i) <- as.zipWithIndex) {
        current = current.linearAssignment(a.v, LinearForm.v(i + input.dimension))
      }
      current.delVariables(input.dimension until current.dimension)
    }
  }

  /**
   * Returns the textual representation of an expression.
   * @param vars symbolic names of variables in the expression.
   */
  def mkString(vars: Seq[String]): Seq[String] = for (a <- as) yield s"${vars(a.v)}' := ${a.exp.mkString(vars)}"

  override def toString = "[ " + mkString(Stream.from(0).map { "v" + _ }).mkString(", ") + " ]"
}

/**
 * The compation object for multiple assignments.
 */
object NumericAssignmentMultiple {
  import scala.language.implicitConversions

  /**
   * Implicit conversion from single assignment to multiple assignment.
   */
  implicit def singleAssignmentToMultiple(ass: NumericAssignment) = NumericAssignmentMultiple(Seq(ass))

  /**
   * Implicit conversion from sequence of assignments to multiple assignment.
   */
  implicit def seqAssignmentToMultiple(as: Seq[NumericAssignment]) = NumericAssignmentMultiple(as)

  /**
   * Constructs a multiple assignments from a list fo assignments given inline.
   */
  def apply(assignment: NumericAssignment, assignments: NumericAssignment*): NumericAssignmentMultiple =
    NumericAssignmentMultiple(assignment +: assignments)

  /**
   * Helper method to detect invalid multiple assignments.
   */
  @annotation.tailrec
  private def containsDups(list: Seq[NumericAssignment], seen: Set[Int] = Set[Int]()): Boolean =
    list match {
      case x :: xs => if (seen.contains(x.v)) true else containsDups(xs, seen + x.v)
      case _ => false
    }

}
  