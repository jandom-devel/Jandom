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

import it.unich.sci.jandom.domains.numerical._

import breeze.linalg.{ DenseMatrix, DenseVector }

/**
 * This object is a collection of transformations between different abstract properties.
 * Each transformation is a map from the original to the target property.
 * @todo evaluate whether it is a good choice to have this object for domain transformations
 */
trait DomainTransformation[A, B] extends Function[A, B]

object DomainTransformation {
  implicit object ParallelotopeToBoxDouble extends DomainTransformation[Parallelotope, BoxDouble] {
    def apply(x: Parallelotope) = {
      val newPar = x.rotate(DenseMatrix.eye(x.dimension))
      if(newPar.isEmpty)
        BoxDouble.bottom(newPar.dimension)
      else
      BoxDouble(newPar.low.toArray, newPar.high.toArray)
    }
  }

  implicit object BoxDoubleToParallelotope extends DomainTransformation[BoxDouble, Parallelotope] {
    def apply(x: BoxDouble) = {
    Parallelotope (DenseVector(x.low),DenseMatrix.eye(x.dimension),DenseVector(x.high))
    }
  }

  implicit object ParallelotopeToParallelotope extends DomainTransformation[Parallelotope, Parallelotope] {
    def apply(x: Parallelotope) = {
    x
    }
  }

  implicit object BoxDoubleToBoxDouble extends DomainTransformation[BoxDouble, BoxDouble] {
    def apply(x: BoxDouble) = {
    x
    }
  }

  implicit object NumericalPropertyToBoxDouble extends DomainTransformation[NumericalProperty[_], BoxDouble] {
    def apply(x: NumericalProperty[_]) = {
      BoxDouble.top(x.dimension)
    }
  }

  /*
  implicit object BoxDoubleToNumericalProperty extends DomainTransformation[BoxDouble,NumericalProperty[_]] {
    def apply(x: BoxDouble) = {
      BoxDouble.full(x.dimension)
    }
  }


  implicit object BoxDoubleToNumericalProperty extends DomainTransformation[BoxDouble,NumericalProperty[_]] {
    def apply(x: BoxDouble) = {
      NumericalProperty[_].full(x.dimension)
    }
  }
*/
}

/*
  val parallelotopeToBoxDouble =  (x: Parallelotope) => {
    val newPar = x.rotate(DenseMatrix.eye(x.dimension))
    BoxDouble(newPar.low.toArray,newPar.high.toArray)
  }

  val boxDoubleToParallelotope =  (x: BoxDouble) => {
    Parallelotope(DenseVector(x.low),DenseMatrix.eye(x.dimension),DenseVector(x.high))
  }
*/
/*    def dom1ToDom2[Prop1 <: AbstractProperty[Prop1]](dom1: Prop1, dom2:BoxDouble):BoxDouble = {
        	dom1 match {
        		case dom1: Parallelotope => {

        		}
        		case dom1: NumericalProperty[_] => BoxDouble.full(dom1.dimension)
        		case _ => BoxDouble.full(0)
        	}
        }

    def dom1ToDom2[Prop1 <: AbstractProperty[Prop1]](dom1: Prop1, dom2: Parallelotope):Parallelotope = {
        	dom1 match {
        		case dom1: BoxDouble =>
        			Parallelotope(DenseVector(dom1.low),DenseMatrix.eye(dom1.dimension),DenseVector(dom1.high))
        		case dom1: NumericalProperty[_] => Parallelotope.full(dom1.dimension)
        		case _ => Parallelotope.full(0)
        	}
    }

    def dom1ToDom2[Prop1 <: AbstractProperty[Prop1]](dom1: Prop1, dom2: AbstractDomain):AbstractDomain = {
    		???
    }

}
*/