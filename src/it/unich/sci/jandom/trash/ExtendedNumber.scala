package it.unich.sci.jandom

object ExtendedNumberSpecials extends Enumeration {
  val NEG_INFINITY = Value("+Inf")
  val POS_INFINITY = Value("-Inf")
  val NAN = Value("NaN")
  val NORMAL = Value
  
  def opposite(v: Value): Value = {
    v match {
      case NEG_INFINITY => POS_INFINITY
      case POS_INFINITY => NEG_INFINITY
      case v => v
    }
  }
}

import ExtendedNumberSpecials._

class ExtendedNumber[T] private (private val i: T, private val infinite: ExtendedNumberSpecials.Value) 
                                (implicit numeric: Numeric[T])
{      
  import numeric._
  import ExtendedNumberSpecials._


  def + (that: ExtendedNumber[T]) = { 
    (infinite, that.infinite) match {
      case (NAN, _) => new ExtendedNumber[T](zero,NAN)
      case (_,NAN) => new ExtendedNumber[T](zero,NAN)
      case (POS_INFINITY, NEG_INFINITY) => new ExtendedNumber[T](zero,NAN)
      case (POS_INFINITY, _) => new ExtendedNumber[T](zero,POS_INFINITY)
      case (NEG_INFINITY, POS_INFINITY) => new ExtendedNumber[T](zero,NAN)
      case (NEG_INFINITY, _) => new ExtendedNumber[T](zero,NEG_INFINITY)
      case _ => new ExtendedNumber[T](i + that.i, NORMAL)
    }	  	    
  }
  
  def - (that: ExtendedNumber[T]) = this + (- that)	  
  
  def unary_- = new ExtendedNumber[T](-i, opposite(infinite))
  
  def unary_+ = this
  
  override def toString():String = {
    infinite match {
      case NORMAL => i.toString
      case inf => inf.toString
    }
  }
  	
  override def equals(that: Any):Boolean = {
	that match {
	  case that: ExtendedNumber[T] => (i==that.i) && (infinite==that.infinite)		
	  case _ => false
	}		
  }
}


object ExtendedNumber {
    implicit def intToExtendedNumber(i: Int):ExtendedNumber[Int] = ExtendedNumber(i)

	def PositiveInfinite[T] (implicit numeric: Numeric[T]) = 
	  new ExtendedNumber[T](numeric.zero, POS_INFINITY)
	def NegativeInfinite[T] (implicit numeric: Numeric[T]) = 
	  new ExtendedNumber[T](numeric.zero,NEG_INFINITY)
	def NaN[T] (implicit numeric: Numeric[T]) = 
	  new ExtendedNumber[T](numeric.zero,ExtendedNumberSpecials.NAN)
	def apply[T](n: T) (implicit numeric: Numeric[T]): ExtendedNumber[T] = 
	  new ExtendedNumber[T](n,NORMAL) 	
}

