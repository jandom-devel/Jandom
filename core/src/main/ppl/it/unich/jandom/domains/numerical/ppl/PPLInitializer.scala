/**
 * Copyright 2013, 2016 Gianluca Amato <gianluca.amato@unich.it>
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

package it.unich.jandom.domains.numerical.ppl

import scala.sys.process.Process

import parma_polyhedra_library.Parma_Polyhedra_Library

/**
 * This is an object whose only purpose is to initialize the PPL library.
 *
 * It should be referred as a first statement by the initialization code of a
 * [[it.unich.jandom.domains.NumericalDomain]] which uses PPL.
 * @author Gianluca Amato <g.amato@unich.it>
 */

private[jandom] object PPLInitializer {
  /**
   * isSuccessful is true when PPL native libraries may be loaded correctly.
   */
  val isSuccessful = try {
    System.loadLibrary("ppl_java")
    true
  } catch {
    case _: UnsatisfiedLinkError =>
      try {
        val path = Process("ppl-config -l").lineStream.head
        System.load(path + "/ppl/libppl_java.so")
        true
      } catch {
        case _: UnsatisfiedLinkError =>
          false
      }
  }

  if (isSuccessful) Parma_Polyhedra_Library.initialize_library()

  override def finalize {
    if (isSuccessful) Parma_Polyhedra_Library.finalize_library()
  }
}
