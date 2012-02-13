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

package it.unich.sci.jandom;
import targets._
import parma_polyhedra_library.{Parma_Polyhedra_Library => PPL}

object Jandom extends App {

  System.load("/usr/local/lib/ppl/libppl_java.so")
  PPL.initialize_library()
   val prog: String = """
    	 xyline <- function(x) {
		    x = 0;
            y = 1;
		    z = 1;
		    if (x<=y) z=4;
		    while (x<=10) x=x+y
          }      
    """
  val parsed = parsers.RandomParser.parseProgram(prog)  
  if (parsed.successful) {
	val program = parsed.get 
	print(program)
    program.analyze(domains.PPLBoxDouble)
    print (program)  
  } else {
    print (parsed)
  }
  
  PPL.finalize_library()
}
