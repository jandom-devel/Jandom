/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

package it.unich.sci.jandom.targets.jvmsoot

import soot._
import it.unich.sci.jandom.targets.Parameters
import scala.collection.mutable.HashMap
import it.unich.sci.jandom.targets.Target

/**
 * @author Gianluca Amato <gamato@unich.it>
 *
 */
trait Interpretation[Tgt <: Target[Tgt], Domain <: Tgt#DomainBase] {
  def apply(method: SootMethod, input: Domain#Property): Domain#Property
}
/*
class TopSootInterpretation[Domain <: SootFrameDomain](val dom: Domain) extends Interpretation[SootCFG[_,_],Domain] {
  import scala.collection.JavaConversions._
  def apply(method: SootMethod, input: dom.Property) = dom.top(method.getReturnType() +: method.getParameterTypes().toSeq.asInstanceOf[Seq[Type]])
}
*/