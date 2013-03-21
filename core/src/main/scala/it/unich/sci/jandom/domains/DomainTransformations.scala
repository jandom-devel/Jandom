package it.unich.sci.jandom.domains

import breeze.linalg._

object DomainTransformations {

  val parallelotopeToBoxDouble =  (x: Parallelotope) => {
    val newPar = x.rotate(DenseMatrix.eye(x.dimension))
    BoxDouble(newPar.low.toArray,newPar.high.toArray)
  } 
  
  val boxDoubleToParallelotope =  (x: BoxDouble) => {
    Parallelotope(DenseVector(x.low),DenseMatrix.eye(x.dimension),DenseVector(x.high))
  }
  
}

