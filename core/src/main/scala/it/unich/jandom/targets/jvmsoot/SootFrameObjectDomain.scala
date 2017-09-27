/**
 * Copyright 2013, 2016 Jandom Team
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

package it.unich.jandom.targets.jvmsoot

import scala.annotation.elidable
import scala.annotation.elidable._

import it.unich.jandom.domains.WideningDescription
import it.unich.jandom.domains.objects.ObjectDomain
import it.unich.jandom.targets.NumericCondition
import it.unich.scalafix.Box
import soot._
import soot.jimple.Constant

/**
 * An abstract frame for analysis of object properties. It depends on an analysis of class reachability
 * which determines whether it is possible to reach a class B from a class A.
 * @author Gianluca Amato <gianluca.amato@unich.it>
 * @author Luca Mangifesta
 * @param dom the basic object domain to use for the analysis
 * @param classAnalysis the analysis of class reachability
 */

class SootFrameObjectDomain(val dom: ObjectDomain[SootObjectModel]) extends SootFrameDomain {

  def top(vars: Seq[Type]) = Property(dom.top(vars), List(vars.reverse: _*), Map())

  def bottom(vars: Seq[Type]) = Property(dom.bottom(vars), List(vars.reverse: _*), Map())

  val widenings = for (w <- dom.widenings) yield WideningDescription(w.name, w.description,
    Box { (a: Property, b: Property) => Property(w(a.prop, b.prop), a.stack, a.globals) })

  /**
   * This class represents a single abstract frame. It tries to support global variables, but it is a kind of hack.
   * @param prop contains the pair sharing properties. Variables are allocated also for non-numerical variables, but they are forced
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
     * Remove the top frame variable.
     */
    private def delUntrackedVariable = Property(prop.delVariable(size - 1), stack.tail, globals)

    /**
     * Add a new variable of a type we do not want to track. This means, we set it to null.
     * @param tpe the type of the variable.
     */
    private def addUntrackedVariable(tpe: Type) = Property(prop.addFreshVariable(tpe).assignNull(size), tpe :: stack, globals)

    def addVariable(tpe: Type) = Property(prop.addVariable(tpe), tpe :: stack, globals)

    def delVariable(m: Int) = Property(prop.delVariable(m), stack.take(dimension - 1 - m) ++ stack.takeRight(m), globals)

    def mapVariables(rho: Seq[Int]) = {
      val newstack = for (i <- rho; if i != -1) yield stack(dimension - 1 - i)
      Property(prop.mapVariables(rho), List(newstack.reverse: _*), globals)
    }

    /**
     * This method check invariants on a numerical abstract frame.
     */
    @elidable(ASSERTION)
    private def invariantCheck() {
      assert(prop.dimension == stack.size, s"Sharing property <${prop}> and stack of types <${stack}> have different dimensions")
      for (i <- 0 until stack.size) stack(size - 1 - i) match {
        case _: RefType =>
        case _ => assert(prop.mustBeNull(i), "A non reference type should be null in the pair sharing component")
      }
    }

    def size = stack.size

    def evalConstant(c: Double) = addUntrackedVariable(DoubleType.v())

    def evalConstant(c: String) = addUntrackedVariable(RefType.v(c.getClass().getName()))

    def evalNull(tpe : Type = soot.NullType.v()) =
      Property(prop.addFreshVariable(tpe).assignNull(size), tpe :: stack, globals)

    def evalNew(tpe: Type) =
      if (tpe.isInstanceOf[RefType])
        Property(prop.addFreshVariable(tpe), tpe :: stack, globals)
      else
        addUntrackedVariable(tpe)

    def evalUnknown(tpe: Type) =
      if (tpe.isInstanceOf[RefType])
        Property(prop.addUnknownVariable(tpe), tpe :: stack, globals)
      else
        addUntrackedVariable(tpe)

    def evalCast(t: soot.Type) = Property(this.prop.castVariable(size - 1, t), t :: stack.tail, globals)

    def evalLocal(v: Int) =
      Property(prop.addFreshVariable(stack(size-1-v)).assignVariable(size, v), stack(size - 1 - v) :: stack, globals)

    def evalLength = addUntrackedVariable(IntType.v())

    def evalField(l: Int, f: SootField) = addUntrackedVariable(f.getType()).assignFieldToStackTop(l, f)

    private def assignFieldToStackTop(src: Int, f: SootField) =
      Property(prop.assignFieldToVariable(size - 1, src, f), stack, globals)

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
    def testLinearCondition(lc: NumericCondition) = (this, this)

    def assignLocal(dst: Int) = {
      if (stack(0).isInstanceOf[RefType])
        Property(prop.assignVariable(dst, size - 1).delVariable(), stack.tail, globals)
      else
        Property(prop.delVariable(), stack.tail, globals)
    }

    def assignField(dst: Int, f: SootField) =
      Property(prop.assignVariableToField(dst, f, size - 1).delVariable(), stack.tail, globals)

    def assignStaticField(dst: Int, f: SootField) =
      Property(prop.assignVariableToField(dst, f, size - 1).delVariable(), stack.tail, globals)

    def mkString(vars: Seq[String]) = prop.mkString(vars) + " // types: " + this.stack.toString

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

    def widening(that: Property) = {
      assert(stack == that.stack)
      Property(prop widening that.prop, stack, globals)
    }
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
