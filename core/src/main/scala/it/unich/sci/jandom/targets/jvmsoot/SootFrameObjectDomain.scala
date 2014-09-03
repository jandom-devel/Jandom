/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>, Francesca Scozzari <fscozzari@unich.it>
 *
 * This file is part of JANDOM: JVM-based Analyzer for Numerical DOMains
 * JANDOM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * JANDOM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of a
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with JANDOM.  If not, see <http://www.gnu.org/licenses/>.
 */

package it.unich.sci.jandom.targets.jvmsoot

import scala.annotation.elidable
import scala.annotation.elidable._

import it.unich.sci.jandom.domains.objects.PairSharingDomain
import it.unich.sci.jandom.domains.objects.UP
import it.unich.sci.jandom.targets.linearcondition.LinearCond
import soot._
import soot.jimple.Constant
import soot.jimple.StaticFieldRef
import it.unich.sci.jandom.domains.objects.ObjectDomain

/**
 * An abstract frame for analysis of object properties. It depends on an analysis of class reachability
 * which determines whether it is possible to reach a class B from a class A.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Luca Mangifesta
 * @param dom the basic object domain to use for the analysis
 * @param classAnalysis the analysis of class reachability
 */

class SootFrameObjectDomain(val dom: ObjectDomain, classAnalysis: ClassReachableAnalysis) extends SootFrameDomain {

  def top(vars: Seq[Type]) = buildProperty(dom.top(vars.size), List(vars.reverse: _*))

  def bottom(vars: Seq[Type]) = buildProperty(dom.bottom(vars.size), List(vars.reverse: _*))

  /**
   * Build a new object property, setting to null those variables which are not of reference
   * type and removing pairs  which cannot share due to class reachability analysis.
   */
  private def buildProperty(prop: dom.Property, stack: List[Type]): Property = {
    val mayShare = { p: UP[Int] =>
      stack(stack.size - 1 - p._1) match {
        case p1: RefType =>
          stack(stack.size - 1 - p._2) match {
            case p2: RefType => classAnalysis.mayShare(p1.getSootClass(), p2.getSootClass())
            case _ => false
          }
        case _ => false
      }
    }
    val currprop = prop filter mayShare
    Property(currprop, stack, Map())
  }

  /**
   * This class represents a single abstract frame. It tries to support global variables, but it is a kind of hack.
   * @param prop contains the pair sharing properties. Variables are allocated also for non-numerical variables, but the are forced
   * to be definitively null.
   * @param stack the stack of variable types in the current frame. Note that stack position are numbered in the
   * opposite way than frame variables, i.e., the frame variable `i` corresponds to stack position `size - 1 - i`.
   * @param globals a map from globals in the JVM to variable positions in `prop`. Used to support global variables.
   */
  case class Property(val prop: dom.Property, val stack: List[Type], val globals: Map[AnyRef, Int]) extends SootFrameProperty[Property] {

    def fiber = stack

    def dimension = stack.length

    type Domain = SootFrameObjectDomain.this.type

    def domain = SootFrameObjectDomain.this

    invariantCheck

    /**
     * Returns the SootClass of a given frame variable
     */
    private def classOfVar(i: Int): SootClass = stack(size - i - 1).asInstanceOf[RefType].getSootClass()

    /**
     * A map UP[Int] => Boolean. mayShare(UP(x,y)) is true if x and y may share according to the class
     * reachability analysis in `classAnalysis`.
     */
    private val mayShare = { p: UP[Int] => classAnalysis.mayShare(classOfVar(p._1), classOfVar(p._2)) }

    /**
     * Remove the top frame variable.
     */
    private def delUntrackedVariable = Property(prop.delVariable(size - 1), stack.tail, globals)

    /**
     * Add a new variable of a type we do not want to track. This means, we set it to null.
     * @param tpe the type of the variable.
     */
    private def addUntrackedVariable(tpe: Type) = Property(prop.addFreshVariable.assignNull(size), tpe :: stack, globals)

    def addVariable(tpe: Type) = Property(prop.addVariable, tpe :: stack, globals)

    def delVariable(m: Int) = Property(prop.delVariable(m), stack.take(dimension - 1 - m) ++ stack.takeRight(m), globals)

    def mapVariables(rho: Seq[Int]) = {
      val newstack = for (i <- rho; if i != -1) yield stack(dimension - 1 - i)
      Property(prop.mapVariables(rho), List(newstack.reverse: _*), globals)
    }

    /**
     * This method check invariants on a numerical abstract frame.
     */
    @elidable(ASSERTION)
    private def invariantCheck {
      assert(prop.dimension == stack.size, s"Sharing property <${prop}> and stack of types <${stack}> have different dimensions")
      for (i <- 0 until stack.size) stack(size - 1 - i) match {
        case _: RefType =>
        case _ => assert(prop.isNull(i), "A non reference type should be null in the pair sharing component")
      }
    }

    def size = stack.size

    def evalConstant(c: Double) = addUntrackedVariable(DoubleType.v())

    def evalConstant(c: String) = addUntrackedVariable(RefType.v(c.getClass().getName()))

    def evalNull =
      Property(prop.addFreshVariable.assignNull(size), NullType.v() :: stack, globals)

    def evalNew(tpe: Type) =
      if (tpe.isInstanceOf[RefType])
        Property(prop.addFreshVariable, tpe :: stack, globals)
      else
        addUntrackedVariable(tpe)

    def evalLocal(v: Int) =
      Property(prop.addFreshVariable.assignVariable(size, v), stack(size - 1 - v) :: stack, globals)

    def evalLength = addUntrackedVariable(IntType.v())

    def evalField(l: Int, f: SootField) = addUntrackedVariable(f.getType()).assignFieldToListTop(l, f)

    private def assignFieldToListTop(src: Int, f: SootField) =
      Property(prop.assignFieldToVariable(size - 1, src, f.getNumber(), mayShare), stack, globals)

    def evalAdd = delUntrackedVariable
    def evalSub = delUntrackedVariable
    def evalMul = delUntrackedVariable
    def evalDiv = delUntrackedVariable
    def evalRem = delUntrackedVariable
    def evalShl = delUntrackedVariable
    def evalShr = delUntrackedVariable
    def evalUshr = delUntrackedVariable
    def evalBinOp = delUntrackedVariable
    def evalNeg = this

    def evalGt = delUntrackedVariable
    def evalGe = delUntrackedVariable
    def evalLt = delUntrackedVariable
    def evalLe = delUntrackedVariable
    def evalEq = delUntrackedVariable
    def evalNe = delUntrackedVariable

    private def test = {
      val dropped = delUntrackedVariable
      (dropped, dropped)
    }

    def testGt = evalGt.test
    def testGe = evalGe.test
    def testLe = evalLe.test
    def testLt = evalLt.test
    def testEq = evalEq.test
    def testNe = evalNe.test

    def evalLinearForm(lf: Array[Double]) = addUntrackedVariable(DoubleType.v())
    def testLinearCondition(lc: LinearCond) = (this, this)

    def assignLocal(dst: Int) = {
      if (stack(0).isInstanceOf[RefType])
        Property(prop.assignVariable(dst, size - 1).delVariable(), stack.tail, globals)
      else
        Property(prop.delVariable(), stack.tail, globals)
    }

    def assignField(dst: Int, f: SootField) =
      Property(prop.assignVariableToField(dst, f.getNumber(), size - 1).delVariable(), stack.tail, globals)

    def assignStaticField(dst: Int, f: SootField) =
      Property(prop.assignVariableToField(dst, f.getNumber(), size - 1).delVariable(), stack.tail, globals)

    def mkString(vars: Seq[String]) = prop.mkString(vars) + "types: " + this.stack.toString

    def union(that: Property) = {
      assert(stack == that.stack)
      Property(prop union that.prop, stack, globals)
    }

    def intersection(that: Property) = {
      assert(stack == that.stack)
      Property(prop union that.prop, stack, globals)
    }

    def extract(n: Int) = {
      assume(n >= 0 && n <= size, s"Trying to extract ${n} variables in the abstract frame {$this}")
      Property(prop.delVariables(0 until size - n), stack.dropRight(size - n), globals)
    }

    def restrict(n: Int) = {
      assume(n >= 0 && n <= size, s"Trying to restrict {n} top variables in the abstract frame {$this}")
      Property(prop.delVariables(size - n until size), stack.drop(n), globals)
    }

    def connect(p: Property, common: Int): Property = Property(prop.connect(p.prop, common), p.stack.dropRight(common) ++ stack.drop(common), globals)

    def widening(that: Property) = this union that

    def narrowing(that: Property) = this intersection that

    def evalGlobal(o: Constant) = {
      val withGlobal = if (globals contains o) this else evalNew(o.getType())
      val evaluatedGlobal = withGlobal.evalLocal(withGlobal.size - 1)
      Property(evaluatedGlobal.prop, evaluatedGlobal.stack, globals + (o -> prop.dimension))
    }

    def evalStaticField(v: SootField): Property = {
      if (v.getType().isInstanceOf[RefType])
        evalNew(v.getType())
      else
        this
    }

    def evalInstance(t: Type): Property = {
      delUntrackedVariable
    }

    def evalSwap(i: Int, j: Int) = ???

    def top = Property(prop.top, stack, globals)

    def bottom = Property(prop.bottom, stack, globals)

    def isTop = prop.isTop

    def isBottom = prop.isBottom

    def isEmpty = prop.isEmpty

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
      other match {
        case other: Property => prop tryCompareTo other.prop
        case _ => None
      }

    override def enterMonitor(n: Int) = Property(prop.testNotNull(n), stack, globals)

    override def exitMonitor(n: Int) = Property(prop.testNotNull(n), stack, globals)

  }

}
