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

trait Interpretation[Tgt <: Target[Tgt]] {
  def apply(params: Parameters[Tgt])(method: SootMethod, input: params.Property): params.Property
}

class TopSootInterpretation[Tgt <: SootCFG[Tgt, _]] extends Interpretation[Tgt] {
  import scala.collection.JavaConversions._
  def apply(params: Parameters[Tgt])(method: SootMethod, input: params.Property): params.Property =
    params.domain.top(method.getReturnType() +: method.getParameterTypes().toSeq.asInstanceOf[Seq[Type]])
}

// this only works for non recursive calls
class JimpleInterpretation extends Interpretation[JimpleMethod] {
    import scala.collection.JavaConversions._

  val inte: scala.collection.mutable.HashMap[(SootMethod, AnyRef), (AnyRef, Boolean)] = scala.collection.mutable.HashMap()

  def apply(params: Parameters[JimpleMethod])(method: SootMethod, input: params.Property): params.Property = {
    if (inte contains (method, input)) inte(method, input) match {
      case (output,true) => output.asInstanceOf[params.Property]
      case (output, false) => throw new IllegalArgumentException("Recursive")
    }
    else {
      val jmethod = new JimpleMethod(method)
      val outFibers = method.getParameterTypes().asInstanceOf[java.util.List[Type]] :+ method.getReturnType()
      inte((method, input)) = (params.domain.bottom(outFibers),false)
      val ann = jmethod.analyzeFromInput(params)(input)
      val output = jmethod.extractOutput(params)(ann)
      inte((method, input)) = (output,true)
      output
    }
  }
}
