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
package it.unich.sci.jandom 

/**
 * This package contains all the abstract domains available in ''Jandom''. There are 
 * two parallel hierarchies: properties and domains. A property is a single abstract
 * object, such a polyhedron or a box. A domain is essentially a factory for abstract
 * objects. 
 * 
 * At the moment, only numerical domains are implemented. The base traits
 * for numerical properties and numerical domains are 
 * [[it.unich.sci.jandom.domains.NumericalProperty]] and [[it.unich.sci.jandom.domains.NumericalDomain]]
 * respectively.   
 * @author Gianluca Amato <amato@sci.unich.it>
 */
package object domains {
	
}