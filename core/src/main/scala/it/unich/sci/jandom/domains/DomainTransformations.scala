/**
 * Copyright 2013 Gianluca Amato, Francesca Scozzari
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

package it.unich.sci.jandom.domains

import it.unich.sci.jandom.domains.numerical.BoxDouble
import it.unich.sci.jandom.domains.numerical.Parallelotope

import breeze.linalg.{DenseMatrix, DenseVector}

/**
 * This object is a collection of transformations between different abstract properties.
 * Each transformation is a map from the original to the target property.
 * @todo evaluate whether it is a good choice to have this object for domain transformations
 */
object DomainTransformations {

  val parallelotopeToBoxDouble =  (x: Parallelotope) => {
    val newPar = x.rotate(DenseMatrix.eye(x.dimension))
    BoxDouble(newPar.low.toArray,newPar.high.toArray)
  }

  val boxDoubleToParallelotope =  (x: BoxDouble) => {
    Parallelotope(DenseVector(x.low),DenseMatrix.eye(x.dimension),DenseVector(x.high))
  }

}

