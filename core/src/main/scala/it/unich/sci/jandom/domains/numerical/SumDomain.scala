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

package it.unich.sci.jandom.domains.numerical

import it.unich.sci.jandom.domains.DomainTransformation

class SumDomain[D1 <: NumericalDomain, D2 <: NumericalDomain](val dom1: D1, val dom2: D2)(
      implicit val dom1Todom2: DomainTransformation[D1,D2], val dom2Todom1: DomainTransformation[D2,D1]) extends NumericalDomain {

  private val pr1 = dom1Todom2(dom1,dom2)
  private val pr2 = dom2Todom1(dom2,dom1)
  
  type Property = Sum

	  /**
	 * This is the class which implements the sum of two abstract properties.
	 * @todo This is only a stub.
	 * @author Gianluca Amato
	 * @author Francesca Scozzari
	 */
  final class Sum(private val p1: dom1.Property, private val p2: dom2.Property)extends NumericalProperty[Sum] {

	require(p1.dimension == p2.dimension)

	type Domain = SumDomain.this.type

    def domain = SumDomain.this

	def union(that: Property): Property = {
	    val q1 = p1 union that.p1
	    val q2 = p2 union that.p2
	    new Sum(q1, q2)
	}


	def widening(that: Property): Property = {
		require(dimension == that.dimension)
		new Sum (p1.widening(that.p1), p2.widening(that.p2))
	}


	def narrowing(that: Sum): Sum = {
	    require(dimension == that.dimension)
	    new Sum (p1.narrowing(that.p1), p2.narrowing(that.p2))
	}


	def intersection(that: Property): Property =
	    throw new IllegalAccessException("Unimplemented feature on Sum")


	def nonDeterministicAssignment(n: Int): Property = {
	    val q1 = p1.nonDeterministicAssignment(n)
	    val q2 = p2.nonDeterministicAssignment(n)
	    if (q1.isTop && q2.isTop) new Sum(q1, q2) else new Sum(if (!q1.isTop) q1 else p1 , if (!q2.isTop) q2 else p2)
	}

    
	def linearAssignment(n: Int, lf: LinearForm[Double]): Property = {
	    val newlf = new DenseLinearForm[Double](lf.known/2 +: lf.homcoeffs)
	    var q1 = p1.linearAssignment(n, newlf)
		var q2 = p2.linearAssignment(n, newlf)
	    if (!q1.isTop && !q2.isTop){
	      return new Sum(q1, q2)
	    }
	    new Sum(p1.top,p2.top)
	}



	def linearInequality(lf: LinearForm[Double]): Property = {
	  if (p1.isEmpty && p2.isEmpty) return this

	  println("\np1= " + p1)
	  println("p2= " + p2)
	 
	  /*
	  if (p1.isTop && p2.isTop)
	  {
	    val new_p1 = p1.intersection(p1.top.linearInequality(new DenseLinearForm[Double]((lf.known/2) +: lf.homcoeffs)))
	    val new_p2 = p2.intersection(p2.top.linearInequality(new DenseLinearForm[Double]((lf.known/2) +: lf.homcoeffs)))
	    //val new_p1 = p1.intersection(p1.top.linearInequality(new DenseLinearForm[Double]((0.0) +: lf.homcoeffs)))
	    //val new_p2 = p2.intersection(p2.top.linearInequality(new DenseLinearForm[Double]((lf.known) +: lf.homcoeffs)))
	    println ("SUM - linearInequality - Entrambi i domini sono a TOP")
	    return new Sum( new_p1 , new_p2 )	    
	  } 
	  	
	  	*      
	  	*/    	  
	  println("SUM - linearInequality - lf: " + lf)
	  println("SUM - linearInequality - lf.dimension: " + lf.dimension)
	  println("SUM - linearInequality - lf.known=" + lf.known)
	  
	  var w1 = p1.minimize(lf) - lf.known
	  var w2 = p2.minimize(lf) - lf.known
	  
	  println("SUM - linearInequality - Minimize su p1: w1= " + w1)
	  println("SUM - linearInequality - Miminize su p2: w2= " + w2)
	  
	  if(!w1.isInfinity && !w2.isInfinity)
	    {
	      val lf_k1 = new DenseLinearForm[Double]((lf.known + w2) +: lf.homcoeffs)
	      val lf_k2 = new DenseLinearForm[Double]((lf.known + w1) +: lf.homcoeffs)
	      println("SUM - linearInequality - lf_k1=" + lf_k1)
	      println("SUM - linearInequality - lf_k2=" + lf_k2)
	      val k1 = p1.top.linearInequality(lf_k1);
	      val k2 = p2.top.linearInequality(lf_k2);
	      println ("SUM - linearInequality - k1= " + k1);
	      println ("SUM - linearInequality - k2= " + k2);
	      
	      val new_p1 = p1.intersection(k1)
	      val new_p2 = p2.intersection(k2)
	      println ("SUM - linearInequality - new_p1= " + new_p1);
	      println ("SUM - linearInequality - new_p2= " + new_p2);
	      
	      new Sum(new_p1, new_p2);
	    }
	    else if (!w1.isInfinity)
	      {
	    	val lf_k2 = new DenseLinearForm[Double]((lf.known + w1) +: lf.homcoeffs)
	        val k2 = p2.linearInequality(lf_k2);
	      	new Sum(p1, p2.intersection(k2));
	      }
	    else if (!w2.isInfinity)
	      {
	    	val lf_k1 = new DenseLinearForm[Double]((lf.known + w2) +: lf.homcoeffs)
	        val k1 = p1.linearInequality(lf_k1);
	      	new Sum(p1.intersection(k1), p2);
	      }
	      
	    else
	    {
	      //se coinvolge solo una variabile
	      var new_p1 = p1
	      var new_p2 = p2
	      if(lf.pairs.size == 1){
	        new_p1 = p1.intersection(p1.top.linearInequality(new DenseLinearForm[Double]((lf.known/2) +: lf.homcoeffs)))
	        new_p2 = p2.intersection(p2.top.linearInequality(new DenseLinearForm[Double]((lf.known/2) +: lf.homcoeffs)))
	      }
	      else
	      {  
	        val a = new DenseLinearForm[Double]((lf.known) +: lf.homcoeffs)
	        val b = p2.top.linearInequality(a)
	        val c = p2.intersection(b)
	        val d = p2.linearInequality(a)
	        
	        println ("SUM - linearInequality - a= " + a);
	        println ("SUM - linearInequality - b= " + b);
	        println ("SUM - linearInequality - c= " + c);
	        println ("SUM - linearInequality - d= " + d);
	        new_p2 = d
	      }
	      println ("SUM - linearInequality - CASO 4 - W1 = W2 = INFINITO");
	      println ("SUM - linearInequality - new_p1= " + new_p1);
	      println ("SUM - linearInequality - new_p2= " + new_p2);
	      new Sum( new_p1 , new_p2 )
	    }
	    
	}


	def linearDisequality(lf: LinearForm[Double]): Property = {
	  this
	}

    def minimize(lf: LinearForm[Double]): Double = ???

    def maximize(lf: LinearForm[Double]): Double = ???

    def frequency(lf: LinearForm[Double]): Some[Double] = ???

	def addVariable: Property = new Sum(p1.addVariable, p2.addVariable)

	def delVariable(n: Int): Property = new Sum(p1.delVariable(n),p2.delVariable(n))

    def mapVariables(rho: Seq[Int]): Property = ???

	def dimension: Int = p1.dimension

	def isTop: Boolean = p1.isTop && p2.isTop
	
	def isEmpty = isBottom

	def isBottom: Boolean = p1.isBottom && p2.isBottom

	def bottom: Property = new Sum(p1.bottom,p2.bottom)

	def top: Property = new Sum(p1.top,p2.top)

    def mkString(vars: Seq[String]): String = {
	  if (isEmpty)
	    "empty"
	  else
	   "FIRST PROP:  " + p1.mkString(vars) + "\n   SECOND PROP: " + p2.mkString(vars)       
    }

	def tryCompareTo[B >: Property](that: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] = that match {
	  case that: Sum => {
        if (p1==that.p1)
          return (p2.tryCompareTo(that.p2))
          else if (p1>that.p1)
            if (p2>=that.p2) return Some(1)
            else return None
         else if(p2<=that.p2) return Some(-1)
            else return None
        None
	  }
	  case _ => None
    }
    
  }

  def top(n: Int) =  new Sum (dom1.top(n), dom2.top(n))
  
  def bottom(n: Int) = new Sum (dom1.bottom(n), dom2.bottom(n))
  
}