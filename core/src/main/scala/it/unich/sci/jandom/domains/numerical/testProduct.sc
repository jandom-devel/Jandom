package it.unich.sci.jandom.domains.numerical

import it.unich.sci.jandom.parsers._
import it.unich.sci.jandom.targets._
import it.unich.sci.jandom.targets.slil._

object testProduct {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val d1 = BoxDouble                              //> d1  : it.unich.sci.jandom.domains.numerical.BoxDouble.type = it.unich.sci.ja
                                                  //| ndom.domains.numerical.BoxDouble$@403a6e15
  val d2 = Parallelotope                          //> d2  : it.unich.sci.jandom.domains.numerical.Parallelotope.type = it.unich.sc
                                                  //| i.jandom.domains.numerical.Parallelotope$@1b31463c
  val n=2                                         //> n  : Int = 2
  
  val productDomain = new ProductDomain(d1, d2)   //> productDomain  : it.unich.sci.jandom.domains.numerical.ProductDomain = it.un
                                                  //| ich.sci.jandom.domains.numerical.ProductDomain@f372a7a
  val v1 = productDomain.full(n)                  //> v1  : it.unich.sci.jandom.domains.numerical.Product[it.unich.sci.jandom.doma
                                                  //| ins.numerical.testProduct.productDomain.dom1.Property,it.unich.sci.jandom.do
                                                  //| mains.numerical.testProduct.productDomain.dom2.Property] = [ (-Infinity <= v
                                                  //| 0 <= Infinity , -Infinity <= v0 <= Infinity) , (-Infinity <= v1 <= Infinity 
                                                  //| , -Infinity <= v1 <= Infinity) ]
         
   // assign v0 = 0
  val v2 = v1.linearAssignment(0, Array(0,0), 0)  //> v2  : it.unich.sci.jandom.domains.numerical.testProduct.v1.Property = [ (0.0
                                                  //|  <= v0 <= 0.0 , 0.0 <= v0 <= 0.0) , (-Infinity <= v1 <= Infinity , -Infinity
                                                  //|  <= v1 <= Infinity) ]
   // assign v1 = v0
  val v3 = v2.linearAssignment(1, Array(1,0), 0)  //> v3  : it.unich.sci.jandom.domains.numerical.testProduct.v2.Property = [ (0.0
                                                  //|  <= v0 <= 0.0 , 0.0 <= v0 <= 0.0) , (0.0 <= v1 <= 0.0 , 0.0 <= -v0+v1 <= 0.0
                                                  //| ) ]
   // v2 = ?
  val v4 = v3.addDimension                        //> v4  : it.unich.sci.jandom.domains.numerical.testProduct.v3.Property = [ (0.0
                                                  //|  <= v0 <= 0.0 , 0.0 <= v0 <= 0.0) , (0.0 <= v1 <= 0.0 , 0.0 <= -v0+v1 <= 0.0
                                                  //| ) , (-Infinity <= v2 <= Infinity , -Infinity <= v2 <= Infinity) ]
 
  val parser = RandomParser()                     //> parser  : it.unich.sci.jandom.parsers.RandomParser = it.unich.sci.jandom.par
                                                  //| sers.RandomParser@5d94f2e8
  val program = "incr <- function () { i=1  while (i<10)  i = i+1 } "
                                                  //> program  : String = "incr <- function () { i=1  while (i<10)  i = i+1 } "
  val program1 = "incr <- function () { i=1  j=i} "
                                                  //> program1  : String = "incr <- function () { i=1  j=i} "
  
  //val numericalDomain = BoxDouble
  val numericalDomain = productDomain             //> numericalDomain  : it.unich.sci.jandom.domains.numerical.ProductDomain = it.
                                                  //| unich.sci.jandom.domains.numerical.ProductDomain@f372a7a
  val params = new Parameters[SLILTarget] { val domain = numericalDomain }
                                                  //> params  : it.unich.sci.jandom.targets.Parameters[it.unich.sci.jandom.targets
                                                  //| .slil.SLILTarget]{val domain: it.unich.sci.jandom.domains.numerical.ProductD
                                                  //| omain} = it.unich.sci.jandom.domains.numerical.testProduct$$anonfun$main$1$$
                                                  //| anon$1@33984c9e
  parser.parseProgram(program) match {
      case parser.Success(program, _) =>
       
       val ann = program.analyze(params)
       Some(params.debugWriter.toString + program.mkString(ann))
 }                                                //> java.lang.IllegalAccessException: Unimplemented feature on Product
                                                  //| 	at it.unich.sci.jandom.domains.numerical.Product.tryCompareTo(Product.sc
                                                  //| ala:141)
                                                  //| 	at scala.math.PartiallyOrdered$class.$greater(PartiallyOrdered.scala:35)
                                                  //| 
                                                  //| 	at it.unich.sci.jandom.domains.AbstractProperty.$greater(AbstractPropert
                                                  //| y.scala:31)
                                                  //| 	at it.unich.sci.jandom.targets.slil.WhileStmt.analyzeStmt(WhileStmt.scal
                                                  //| a:106)
                                                  //| 	at it.unich.sci.jandom.targets.slil.CompoundStmt$$anonfun$analyzeStmt$1.
                                                  //| apply(CompoundStmt.scala:39)
                                                  //| 	at it.unich.sci.jandom.targets.slil.CompoundStmt$$anonfun$analyzeStmt$1.
                                                  //| apply(CompoundStmt.scala:36)
                                                  //| 	at scala.collection.immutable.List.foreach(List.scala:318)
                                                  //| 	at it.unich.sci.jandom.targets.slil.CompoundStmt.analyzeStmt(CompoundStm
                                                  //| t.scala:36)
                                                  //| 	at it.unich.sci.jandom.targets.slil.SLILProgram.analyze(SLILProgram.scal
                                                  //| a:53)
                                                  //| 	at it.unich.sci.jandom.domains.numerical.testProduct$$anonfun$main$1.app
                                                  //| ly
                                                  //| Output exceeds cutoff limit.
 

}