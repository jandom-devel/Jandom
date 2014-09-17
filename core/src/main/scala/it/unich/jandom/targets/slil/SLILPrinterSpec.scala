package it.unich.jandom.targets.slil

import it.unich.jandom.domains.numerical.NumericalProperty
import it.unich.jandom.targets.Environment

/**
 * A SLILPrinterSpec help in printing a SLIL program with accompanying assertions.
 * @author Gianluca Amato <gamato@unich.it>
 */
abstract class SLILPrinterSpec {
  /**
   * The number of spaces for each indentation level.
   */
  val indentWidth: Int

  /**
   * An environment used to print properties.
   */
  def env: Environment

  /**
   * Generates the correct number of spaces for the given indentation level.
   */
  def indent(level: Int): String = { " " * level * indentWidth }

  /**
   * Decorator is a function which takes an annotation (of any kind), and returns its string representation.
   * If the result is None, we want to omit the property from the output.
   * @param p the property to output
   * @param row the row of the program which p refers to
   * @param col the column of the program which p refers to
   */
  def decorator (p: NumericalProperty[_], row: Int, col: Int): Option[String]
}
