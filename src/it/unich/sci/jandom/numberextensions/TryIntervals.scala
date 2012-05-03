package it.unich.sci.jandom
package numberextensions

object TryIntervals {

  def main(args : Array[String]) : Unit = {
	  val ei = IntegerExt(10)
	  val ej = IntegerExt.PositiveInfinity	  	  
	  println(ei+ei)
	  println(ei)
	  println(ej)
	  println(-ei+ IntegerExt(5))
	  println(ei+ej)
	  println(ej + IntegerExt(2))
	  println(ei + 2)
	  println(ei - 2)
	  //println(2 + ei)
	  println(-ej)
	  println(+ei)	 
		  
	  val e = IntegerExt.PositiveInfinity
	  e match {
	    case IntegerExt.PositiveInfinity => println("OK")
	    case IntegerExt(n) => println(n)
	  }
  }
   
}
