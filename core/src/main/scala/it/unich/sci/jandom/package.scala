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

package it.unich.sci

/**
 * ''Jandom'' is a framework for developing static analysis tools based on [[http://www.di.ens.fr/~cousot/AI/ ''Abstract Interpretation'']].
 * @author Gianluca Amato <amato@sci.unich.it>
 *
 */
package object jandom {
  /**
   * This is a string representation of Jandom's version
   */
  val version = BuildInfo.version

  private val versionRegEx = """(\d*)\.(\d*)\.(\d*)-(.*)""".r
  private val versionRegEx(major, minor, patchlevel, suffix) = version

  /**
   * Major version
   */
  val majorVersion = major.toInt

  /**
   * Minor version. Two releases with the same minor version should
   * be compatible.
   */
  val minorVerion = minor.toInt

  /**
   * Patch level
   */
  val patchlevelVersion = patchlevel.toInt

  /**
   * Suffix Version. This is empty for releases and should be a version system
   * specific release number for development builds (such as commit number for
   * for git)
   */
  val suffixVersion = suffix

  /**
   * This is the name of the software
   */
  val softwareName = "Jandom"
}
