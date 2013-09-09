/**
 * Copyright 2013 Gianluca Amato
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

package it.unich.sci.jandom.domains.numerical.ppl

import parma_polyhedra_library.Coefficient
import parma_polyhedra_library.Linear_Expression
import parma_polyhedra_library.Linear_Expression_Coefficient
import parma_polyhedra_library.Linear_Expression_Variable
import parma_polyhedra_library.Variable
import java.util.regex.Matcher
import it.unich.sci.jandom.domains.numerical.LinearForm

/**
 * This is a collection of methods used by the PPL-based numerical domains.
 * @author Gianluca Amato <g.amato@unich.it>
 */
private[jandom] object PPLUtils {
  /**
   * This is an utility method used by the mkString methods of the PPL-based
   * numerical domains. Since the Java API is not very reach, the only way
   * to realize `mkString` is to use the standard `toString` method and replace
   * the letters `A`, `B`, etc... with the corresponding variable name. This is what
   * this method does.
   * @param pplout a string obtained as the result of a `toString` method for a PPL object
   * @param vars a sequence of variable names to replace for `A`, `B`, etc...
   * @result the output `pplout` converted in the `mkString` format
   */
  def replaceOutputWithVars(pplout: String, vars: Seq[String]): String = {
    var str = pplout
    for (i <- 0 until vars.length) {
      val letter = i % ('Z'-'A' + 1)
      val number = i / ('Z'-'A' + 1)
      val pplvar = (letter+'A').toChar.toString + (if (number!=0) number + " " else " ")
      str = str.replaceAll(pplvar, Matcher.quoteReplacement(vars(i))+" ")
    }
    str
  }

  /**
   * Converts a sequence of homogeneous and in-homogeneous coefficients into a `Linear_Expression`
   * object.
   * @param coeff the homogeneous coefficients.
   * @param known the in-homogeneous coefficient.
   */
  def toPPLLinearExpression(lf: LinearForm[Double]): Linear_Expression = {
    var le : Linear_Expression = new Linear_Expression_Coefficient(new Coefficient(lf.known.toInt))
	for (i <- 0 until lf.dimension) {
	  le = le.sum ( (new Linear_Expression_Variable(new Variable(i)).times(new Coefficient(lf.homcoeffs(i).toInt)) ))
	}
    return le
  }
}