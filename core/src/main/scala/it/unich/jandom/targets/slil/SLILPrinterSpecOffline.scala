package it.unich.jandom.targets.slil

import it.unich.jandom.targets.Environment
import it.unich.jandom.domains.numerical.NumericalProperty
import scala.collection.mutable.Buffer

/**
 * This is a SLILPrinterSpec do not prints the annotations inline. Instead, information on the
 * position of annotations (row/column) is available to the user.
 * @author Gianluca Amato <gamato@unich.it>
 */
class SLILPrinterSpecOffline(val env: Environment, val indentWidth: Int) extends SLILPrinterSpec {
  val annotations = Buffer[(Int, Int, String)]()

  def decorator (p: NumericalProperty[_], row: Int, col: Int): Option[String] = {
    annotations += ((row,col,p.mkString(env.variables)))
    Some("")
  }
}

/**
 * The companion object for AnnotationPrinterSpec
 */
object SLILPrinterSpecOffline {

  /**
   * Builds a pretty printer specification with all the default values
   */
  def apply(env: Environment, indentWidth: Int = 2) = new SLILPrinterSpecOffline(env, indentWidth)
}
