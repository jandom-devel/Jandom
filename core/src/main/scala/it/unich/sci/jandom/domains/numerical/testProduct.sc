package it.unich.sci.jandom.domains.numerical

import it.unich.sci.jandom.parsers._
import it.unich.sci.jandom.targets._
import it.unich.sci.jandom.targets.slil._
import it.unich.sci.jandom.domains.DomainTransformation
//import it.unich.sci.jandom.domains._
//import it.unich.sci.jandom.domains.numerical.BoxDouble

object testProduct {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
 
  val d1 = BoxDouble                              //> d1  : it.unich.sci.jandom.domains.numerical.BoxDouble.type = it.unich.sci.ja
                                                  //| ndom.domains.numerical.BoxDouble$@4110f319
//val d1 = Parallelotope
  
  val d2 = Parallelotope                          //> d2  : it.unich.sci.jandom.domains.numerical.Parallelotope.type = it.unich.sc
                                                  //| i.jandom.domains.numerical.Parallelotope$@10f7ef9f
  val n=2                                         //> n  : Int = 2

  val productDomain = new ProductDomain {
    val dom1 = d1
    val dom2 = d2
  	val dom1Todom2 = implicitly[DomainTransformation[d1.Property, d2.Property]] // BoxDoubleToParallelotope
  	val dom2Todom1 = implicitly[DomainTransformation[d2.Property, d1.Property]] // ParallelotopeToBoxDouble
  }                                               //> productDomain  : it.unich.sci.jandom.domains.numerical.ProductDomain{val dom
                                                  //| 1: it.unich.sci.jandom.domains.numerical.BoxDouble.type; val dom2: it.unich.
                                                  //| sci.jandom.domains.numerical.Parallelotope.type} = it.unich.sci.jandom.domai
                                                  //| ns.numerical.testProduct$$anonfun$main$1$$anon$1@280352ec
  
  
 productDomain.dom1Todom2                         //> res0: it.unich.sci.jandom.domains.DomainTransformation[it.unich.sci.jandom.d
                                                  //| omains.numerical.testProduct.productDomain.dom1.Property,it.unich.sci.jandom
                                                  //| .domains.numerical.testProduct.productDomain.dom2.Property] = <function1>
 
  val p1= new productDomain.ProductProperty(BoxDouble.top(n), Parallelotope.bottom(n))
                                                  //> p1  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Produc
                                                  //| tProperty = [ [voidProduct] ]
  
  val box = BoxDouble(Array(1, 2), Array(5, 4))   //> box  : it.unich.sci.jandom.domains.numerical.BoxDouble = [ 1.0 <= v0 <= 5.0 
                                                  //| , 2.0 <= v1 <= 4.0 ]

  val p2= new productDomain.ProductProperty(box, Parallelotope.top(n))
                                                  //> p2  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Produ
                                                  //| ctProperty = [ (1.0 <= v0 <= 5.0 , -Infinity <= v0 <= Infinity) , (2.0 <= v
                                                  //| 1 <= 4.0 , -Infinity <= v1 <= Infinity) ]
   
  val boxEmpty =  BoxDouble.bottom(n)             //> boxEmpty  : it.unich.sci.jandom.domains.numerical.BoxDouble = [ [void] ]
  boxEmpty.isEmpty                                //> res1: Boolean = true
  
  val ptopeFull = Parallelotope.top(n)            //> ptopeFull  : it.unich.sci.jandom.domains.numerical.Parallelotope = [ -Infin
                                                  //| ity <= v0 <= Infinity , -Infinity <= v1 <= Infinity ]
  ptopeFull.isFull                                //> res2: Boolean = true
  
  
  val p3= new productDomain.ProductProperty(boxEmpty, ptopeFull)
                                                  //> p3  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Produ
                                                  //| ctProperty = [ [voidProduct] ]
                      
 p3.isEmpty                                       //> res3: Boolean = true
  val x1 = productDomain.top(n)                   //> x1  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Produ
                                                  //| ctProperty = [ (-Infinity <= v0 <= Infinity , -Infinity <= v0 <= Infinity) 
                                                  //| , (-Infinity <= v1 <= Infinity , -Infinity <= v1 <= Infinity) ]
   // assign v0 = 1
  val x2 = x1.linearAssignment(0, Array(0,0), 1)  //> x2  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Prope
                                                  //| rty = [ (v0 = 1.0 , v0 = 1.0) , (-Infinity <= v1 <= Infinity , -Infinity <=
                                                  //|  v1 <= Infinity) ]
              
   // assign v1 = v0
  val x3 = x2.linearAssignment(1, Array(1,0), 0)  //> x3  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Prope
                                                  //| rty = [ (v0 = 1.0 , v0 = 1.0) , (v1 = 1.0 , -v0+v1 = 0.0) ]
         
   // v2 = ?
  val x4 = x3.addVariable                         //> x4  : it.unich.sci.jandom.domains.numerical.testProduct.productDomain.Prope
                                                  //| rty = [ (v0 = 1.0 , v0 = 1.0) , (v1 = 1.0 , -v0+v1 = 0.0) , (-Infinity <= v
                                                  //| 2 <= Infinity , -Infinity <= v2 <= Infinity) ]
  
  x4.minimize(Array(1,0,0), 1)                    //> res4: Double = 2.0
  x4.p1.minimize(Array(1,0,0), 1)                 //> res5: Double = 2.0
  x4.p2.minimize(Array(1,0,0), 1)                 //> res6: Double = -Infinity
   
  val parser = RandomParser()                     //> parser  : it.unich.sci.jandom.parsers.RandomParser = it.unich.sci.jandom.pa
                                                  //| rsers.RandomParser@78871dc5
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
                                                  //| domains.numerical.testProduct$$anonfun$main$1$$anon$1@280352ec
                
 
  val params = new Parameters[SLILTarget] { val domain = numericalDomain }
                                                  //> params  : it.unich.sci.jandom.targets.Parameters[it.unich.sci.jandom.target
                                                  //| s.slil.SLILTarget]{val domain: it.unich.sci.jandom.domains.numerical.Produc
                                                  //| tDomain{val dom1: it.unich.sci.jandom.domains.numerical.BoxDouble.type; val
                                                  //|  dom2: it.unich.sci.jandom.domains.numerical.Parallelotope.type}} = it.unic
                                                  //| h.sci.jandom.domains.numerical.testProduct$$anonfun$main$1$$anon$2@96ba55a
  parser.parseProgram(program) match {
      case parser.Success(program, _) =>
       
      val  ann = program.analyze(params)
      Some(params.debugWriter.toString + program.mkString(ann))
 }                                                //> res7: Some[String] = Some(function () {
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