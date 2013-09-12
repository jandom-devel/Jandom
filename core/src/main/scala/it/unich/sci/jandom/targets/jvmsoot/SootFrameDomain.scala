/**
 * Copyright 2013 Gianluca Amato <gamato@unich.it>
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

package it.unich.sci.jandom.targets.jvmsoot

import scala.collection.immutable.Stack
import it.unich.sci.jandom.domains.AbstractDomain
import it.unich.sci.jandom.domains.AbstractProperty
import it.unich.sci.jandom.targets.linearcondition.LinearCond
import soot._
import soot.jimple.Constant
import soot.jimple.FieldRef
import soot.jimple.StaticFieldRef

/**
 * This is the base trait domain for the analysis of methods using Soot. It represents
 * an abstract typed frame with locals and stack. It is used for both Baf and Jimple analysis,
 * although Jimple could be made faster by analyzing one statement at a time instead of
 * evaluating expressions compositionally.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Luca Mangifesta
 */
trait SootFrameDomain extends AbstractDomain {
  type Property <: SootFrameProperty[Property]

  /**
   * Returns the top abstract property relative to the fiber `vars`
   */
  def top(vars: Seq[Type]): Property

  /**
   * Returns the bottom abstract property relative to the fiber `vars`
   */
  def bottom(vars: Seq[Type]): Property

  /**
   * This trait represents a single abstract frame. It behaves like a stack, with numbered positions.
   * Each position is numbered with its index (starting from the bottom, which as index 0).
   * @tparam Property the target class of F-bounded polymporphism
   */
  trait SootFrameProperty[Property <: SootFrameProperty[Property]] extends AbstractProperty[Property] {
    this: Property =>

    /**
     * Determines whether it is possible to assign a variable of type t2
     * to a variable of type t1. It is only used in debugging assertions.
     */
    protected def compatibleTypes(t1: Type, t2: Type): Boolean = {
      t1 == t2 ||
        (t1.isInstanceOf[PrimType] && t2.isInstanceOf[PrimType]) ||
        (t1.isInstanceOf[RefType] && t2.isInstanceOf[RefType])
    }

    /**
     * Return the numbers of variables in the frame
     */
    def size: Int

    /**
     * Push a constant into the frame
     * @param c constant to push into the frame
     */
    def evalConstant(c: Double): Property //era Int

    /**
     * Push a constant into the frame
     * @param c constant to push into the frame
     */
    def evalConstant(c: String): Property

    /**
     * Push a null constant into the frame
     */
    def evalNull: Property

    /**
     * Push a new object into the frame
     * @param tpe the type of the new object
     */
    def evalNew(tpe: Type): Property

    /**
     * Evaluate  a frame variable `i` and push a copy into the frame
     * @param i the frame variable to evaluate
     */
    def evalLocal(i: Int): Property

    /**
     * Evaluate a field of a frame variable, and push a copy into the frame
     * @param i the frame variable to evaluate
     * @param f the field to evaluate within `i`
     */
    def evalField(i: Int = size - 1, f: SootField): Property

    /**
     * Sums the two top element of the frame and replace them with the result
     */
    def evalAdd: Property

    /**
     * Subtracts the top element of the frame from the element `size`-2 and replace them with the result.
     */
    def evalSub: Property

    /**
     * Multiplies the two top element of the frame and replace them with the result.
     */
    def evalMul: Property

    /**
     * Divides the element `size`-2 with the top element of the frame and replace both of them with the result.
     */
    def evalDiv: Property

    /**
     * Sum an integer constant to the frame variable 'i' and replace this last whit the result
     */
    def evalInc(i: Int, c: Double): Property = evalLocal(i).evalConstant(c).evalAdd.assignLocal(i)

    /**
     * Computes the reminder of dividing the element `size`-2 with the top element, and replace them with the result.
     */
    def evalRem: Property

    /**
     * Computes el(size-2) << el(size-1) and replace the two top elements of the stack with the result.
     */
    def evalShl: Property

    /**
     * Computes el(size-2) >> el(size-1) and replace the two top elements of the stack with the result.
     */
    def evalShr: Property

    /**
     * Computes el(size-2) >>> el(size-1) and replace the two top elements of the stack with the result.
     */
    def evalUshr: Property

    /**
     * Computes the effect of a non-specified operations between el(size-2) and el(size-1), and replace
     * the two top elements of the stack with the result.
     */
    def evalBinOp: Property

    /**
     * Replace the top of the stack with its opposite.
     */
    def evalNeg: Property

    /**
     * Replace the top of the stack (which should be an array object) with its length.
     */
    def evalLength: Property

    /**
     * Returns the abstract frame obtained by restricting the current frame to the
     * case when el(size-2) > el(size-1). Remove the the two top elements.
     */
    def evalGt: Property

    /**
     * Returns the abstract frame obtained by restricting the current frame to the
     * case when el(size-2) >= el(size-1). Remove the the two top elements.
     */
    def evalGe: Property

    /**
     * Returns the abstract frame obtained by restricting the current frame to the
     * case when el(size-2) < el(size-1). Remove the the two top elements.
     */
    def evalLt: Property

    /**
     * Returns the abstract frame obtained by restricting the current frame to the
     * case when el(size-2) <= el(size-1). Remove the the two top elements.
     */
    def evalLe: Property

    /**
     * Returns the abstract frame obtained by restricting the current frame to the
     * case when el(size-2) == el(size-1). Remove the the two top elements.
     */
    def evalEq: Property

    /**
     * Returns the abstract frame obtained by restricting the current frame to the
     * case when el(size-2) != el(size-1). Remove the the two top elements.
     */
    def evalNe: Property

    /**
     * Assign to the variable frame `i` the value at the top of the frame, and pop it.
     * @param i the frame variable to assign
     */
    def assignLocal(i: Int): Property

    /**
     * Assign the variable `src` to the variable `dst`
     * @param dst the target variable
     * @param src the source variable
     */
    def assignLocal(dst: Int, src: Int): Property

    /**
     * Assign to  a field of the variable frame `i` the value at the top of the frame, and pop it.
     * @param i the frame variable to assign
     * @param f the field to assign
     */
    def assignField(i: Int = 0, f: SootField): Property

    /**
     * Assign to  a static field of the variable frame `i` the value at the top of the frame, and pop it.
     * @param i the frame variable to assign
     * @param f the field to assign
     */
    def assignStaticField(i: Int = 0, f: SootField): Property

    /**
     * Returns the result of testing whether el(size-2) > el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testGt: (Property, Property)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testGe: (Property, Property)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testLt: (Property, Property)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testLe: (Property, Property)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testEq: (Property, Property)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testNe: (Property, Property)

    /**
     * Returns the result of testing the frame w.r.t. a linear condition.
     * @param lf the linear condition to use for testing.
     */
    def testLinearCondition(lf: LinearCond): (Property, Property)

    /**
     * Returns the last `n` dimensions.
     */
    def restrict(n: Int): Property

    /**
     * Remove the last or the two last dimension of the frame.
     * @param n the number of dimensions to be removed. We assume n is between 1 and 2
     */
   // def restrictTop(n: Int): Property
    
    /**
     * Connect `this` with `p`, keeping in consideration that the last `common` dimensions
     * in `this` and the first `common` dimensions in `p` are the same. The common dimensions
     * are removed after the connection is completed.
     */
    def connect(p: Property, common: Int): Property

    /**
     * Evaluates the effect of entering a monitor.
     * @param n the frame variable whose monitor is entered.
     */
    def enterMonitor(n: Int = size - 1): Property

    /**
     * Evaluates the effect of exiting a monitor.
     * @param n the frame variable whose monitor is exited.
     */
    def exitMonitor(n: Int = size - 1): Property

    /**
     * Evaluates a global constant and put the result on the frame.
     * @param o the constant to evaluate.
     */
    def evalGlobal(o: Constant): Property

    /**
     * Evaluate a static field and put the result on the frame.
     * @param v the static field to evaluate.
     */
    def evalStaticField(v: SootField): Property

    /**
     * Exchange the position of the top element of the frame with that situate in the position i.
     */
    def evalSwap(i: Int = size - 2, j: Int = size - 1): Property

    /**
     * Create a duplicate of the top element of the frame and insert it on the top of the frame
     */
    def evalDup1(): Property = evalLocal(size - 1)

    /**
     * Create a duplicate of the top element of the frame and insert it two position under the top of the frame
     */
    def evalDup1_x1(): Property = evalSwap().evalLocal(size - 2)

    /**
     * Create a duplicate of the top element of the frame and insert it three position under the top of the frame
     */
    def evalDup1_x2(): Property = evalSwap(size - 3, size - 1).evalSwap().evalLocal(size - 3)

    /**
     * Create two duplicate of the two top element of the frame and insert them on the top of the frame
     */
    def evalDup2(): Property = evalLocal(size - 2).evalLocal(size - 1)

    /**
     * Create two duplicate of the two top element of the frame and insert them three position under the top of the frame
     */
    def evalDup2_x1(): Property = evalSwap(size - 3, size - 2).evalSwap().evalLocal(size - 3).evalLocal(size - 3)

    /**
     * Create two duplicate of the two top element of the frame and insert them four position under the top of the frame
     */
    def evalDup2_x2(): Property = evalSwap(size - 4, size - 2).evalSwap(size - 3, size - 1).evalLocal(size - 4).evalLocal(size - 4)

    def evalInstance(t: Type): Property

    /**
     * Returns a string representation of the abstract property.
     * @param vars an array with the name of the variables in the environment
     * @return a sequence of strings. The idea is that each string is an atomic piece of information
     * which should be printed out together, while different strings may be also printed out
     * separately.
     */
    def mkString(vars: Seq[String]): String

    /**
     * Returns the string representation of the property. It calls `mkString` with the standard
     * variable names `v1` ... `vn`.
     */
    override def toString = mkString(for (i <- 0 until size) yield "v" + i)
  }
}
