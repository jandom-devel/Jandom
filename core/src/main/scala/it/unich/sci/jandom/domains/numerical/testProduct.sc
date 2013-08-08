package it.unich.sci.jandom.domains.numerical

import it.unich.sci.jandom.parsers._
import it.unich.sci.jandom.targets._
import it.unich.sci.jandom.targets.slil._
import it.unich.sci.jandom.domains.DomainTransformation._
import it.unich.sci.jandom.domains._

object testProduct {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
 
  val d1 = BoxDouble                              //> d1  : it.unich.sci.jandom.domains.numerical.BoxDouble.type = it.unich.sci.ja
                                                  //| ndom.domains.numerical.BoxDouble$@482ef8a9
 // val d1 = Parallelotope
  
  val d2 = Parallelotope                          //> d2  : it.unich.sci.jandom.domains.numerical.Parallelotope.type = it.unich.sc
                                                  //| i.jandom.domains.numerical.Parallelotope$@fba3c49
  val n=2                                         //> n  : Int = 2
  
  val productDomain = new ProductDomain {
    val dom1 = d1
    val dom2 = d2
  	val dom1Todom2 = implicitly[DomainTransformation[d1.Property, d2.Property]] // BoxDoubleToParallelotope
  	val dom2Todom1 = implicitly[DomainTransformation[d2.Property, d1.Property]] // ParallelotopeToBoxDouble
  }                                               //> productDomain  : it.unich.sci.jandom.domains.numerical.ProductDomain{val dom
                                                  //| 1: it.unich.sci.jandom.domains.numerical.BoxDouble.type; val dom2: it.unich.
                                                  //| sci.jandom.domains.numerical.Parallelotope.type} = it.unich.sci.jandom.domai
                                                  //| ns.numerical.testProduct$$anonfun$main$1$$anon$1@2af21f19
  val v1 = productDomain.full(n)                  //> v1  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Proper
                                                  //| ty = [ (-Infinity <= v0 <= Infinity , -Infinity <= v0 <= Infinity) , (-Infin
                                                  //| ity <= v1 <= Infinity , -Infinity <= v1 <= Infinity) ]
         
   // assign v0 = 0
  val v2 = v1.linearAssignment(0, Array(0,0), 0)  //> v2  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Proper
                                                  //| ty = [ (v0 = 0.0 , v0 = 0.0) , (-Infinity <= v1 <= Infinity , -Infinity <= v
                                                  //| 1 <= Infinity) ]
              
   // assign v1 = v0
  val v3 = v2.linearAssignment(1, Array(1,0), 0)  //> v3  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Proper
                                                  //| ty = [ (v0 = 0.0 , v0 = 0.0) , (v1 = 0.0 , -v0+v1 = 0.0) ]
         
   // v2 = ?
  val v4 = v3.addDimension                        //> v4  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Proper
                                                  //| ty = [ (v0 = 0.0 , v0 = 0.0) , (v1 = 0.0 , -v0+v1 = 0.0) , (-Infinity <= v2 
                                                  //| <= Infinity , -Infinity <= v2 <= Infinity) ]
  
  val parser = RandomParser()                     //> parser  : it.unich.sci.jandom.parsers.RandomParser = it.unich.sci.jandom.par
                                                  //| sers.RandomParser@58596d12
  val program2 = "incr <- function () { j=1 i=1  while (i<10)  { i = i+1} j=i} "
                                                  //> program2  : String = "incr <- function () { j=1 i=1  while (i<10)  { i = i+
                                                  //| 1} j=i} "
  val program1 = "incr <- function () { i=1 } "   //> program1  : String = "incr <- function () { i=1 } "
 
val program = "f <- function() {x=1 y=1 while (y < 100) {y=y+y y=y+y x=x+x x=x+x } } "
                                                  //> program  : String = "f <- function() {x=1 y=1 while (y < 100) {y=y+y y=y+y 
                                                  //| x=x+x x=x+x } } "
 // val numericalDomain = BoxDouble
// val numericalDomain = Parallelotope
 val numericalDomain = productDomain              //> numericalDomain  : it.unich.sci.jandom.domains.numerical.ProductDomain{val 
                                                  //| dom1: it.unich.sci.jandom.domains.numerical.BoxDouble.type; val dom2: it.un
                                                  //| ich.sci.jandom.domains.numerical.Parallelotope.type} = it.unich.sci.jandom.
                                                  //| domains.numerical.testProduct$$anonfun$main$1$$anon$1@2af21f19
                 
 
  val params = new Parameters[SLILTarget] { val domain = numericalDomain }
                                                  //> params  : it.unich.sci.jandom.targets.Parameters[it.unich.sci.jandom.target
                                                  //| s.slil.SLILTarget]{val domain: it.unich.sci.jandom.domains.numerical.Produc
                                                  //| tDomain{val dom1: it.unich.sci.jandom.domains.numerical.BoxDouble.type; val
                                                  //|  dom2: it.unich.sci.jandom.domains.numerical.Parallelotope.type}} = it.unic
                                                  //| h.sci.jandom.domains.numerical.testProduct$$anonfun$main$1$$anon$2@28af778e
                                                  //| 
  parser.parseProgram(program) match {
      case parser.Success(program, _) =>
       
      val  ann = program.analyze(params)
      Some(params.debugWriter.toString + program.mkString(ann))
 }                                                //> res0: Some[String] = Some(function () {
                                                  //|   [(-Infinity <= x <= Infinity , x = 1.0) , (-Infinity <= y <= Infinity , -
                                                  //| Infinity <= y <= Infinity)]
                                                  //|   x = 1
                                                  //|   [(x = 1.0 , x = 1.0) , (-Infinity <= y <= Infinity , y = 1.0)]
                                                  //|   y = 1
                                                  //|   [(x = 1.0 , x = 1.0) , (y = 1.0 , y = 1.0)]
                                                  //|   while (-100+y<0) [(1.0 <= x <= Infinity , 1.0 <= x <= Infinity) , (1.0 <=
                                                  //|  y <= 400.0 , 1.0 <= y <= 400.0)] {
                                                  //|     [(1.0 <= x <= Infinity , 1.0 <= x <= Infinity) , (1.0 <= y <= 100.0 , 1
                                                  //| .0 <= y <= 100.0)]
                                                  //|     y = 2*y
                                                  //|     [(1.0 <= x <= Infinity , 1.0 <= x <= Infinity) , (2.0 <= y <= 200.0 , 1
                                                  //| .0 <= 0.5*y <= 100.0)]
                                                  //|     y = 2*y
                                                  //|     [(1.0 <= x <= Infinity , 1.0 <= x <= Infinity) , (4.0 <= y <= 400.0 , 1
                                                  //| .0 <= 0.25*y <= 100.0)]
                                                  //|     x = 2*x
                                                  //|     [(2.0 <= x <= Infinity , 1.0 <= 0.5*x <= Infinity) , (4.0 <= y <= 400.0
                                                  //|  , 1.0 <= 0.25*y <= 100.0)]
                                                  //|     x = 2*x
                                                  //|     [(4.0 <= x <= Infinity , 1.0 <= 0.25*x <= Infinity) , (4.0 <= y <= 400.
                                                  //| 0 , 1.0 <= 0.25*y <= 100.0)]
                                                  //|   }
                                                  //|   [(1.0 <= x <= Infinity , 1.0 <= x <= Infinity) , (100.0 <= y <= 400.0 , 1
                                                  //| 00.0 <= y <= 400.0)]
                                                  //| }
                                                  //| )
                                         
                               
      
        
                
}