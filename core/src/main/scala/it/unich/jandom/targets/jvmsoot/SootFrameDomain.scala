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

package it.unich.jandom.targets.jvmsoot

import it.unich.jandom.targets.NumericCondition
import it.unich.jandom.domains.AbstractDomain
import it.unich.jandom.domains.AbstractProperty
import it.unich.jandom.domains.CartesianFiberedProperty
import it.unich.jandom.domains.CartesianFiberedDomain
import soot.NullType

/**
 * This is the base trait domain for the analysis of methods using Soot. It represents
 * an abstract typed frame with locals and stack. It is used for both Baf and Jimple analysis,
 * although Jimple could be made faster by analyzing one statement at a time instead of
 * evaluating expressions compositionally.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Luca Mangifesta
 */
trait SootFrameDomain extends CartesianFiberedDomain {

  type FiberComponent = soot.Type

  type Property <: SootFrameProperty[Property]

  /**
   * Determines whether it is possible to assign a variable of type t2
   * to a variable of type t1. It is only used in debugging assertions.
   */
  protected def compatibleTypes(t1: soot.Type, t2: soot.Type): Boolean = {
    t1 == t2 ||
      (t1.isInstanceOf[soot.PrimType] && t2.isInstanceOf[soot.PrimType]) ||
      (t1.isInstanceOf[soot.RefType] && t2.isInstanceOf[soot.RefType])
  }

  /**
   * This trait represents a single abstract frame.
   * @author Gianluca Amato <gamato@unich.it>
   * @author Luca Mangifesta
   */
  trait SootFrameProperty[P <: SootFrameProperty[P]] extends CartesianFiberedProperty[soot.Type, P] {
    this: P =>

    /**
     * Returns the last `n` dimensions.
     */
    def extract(n: Int): P

    /**
     * Deletes the last `n` dimensions
     */
    def restrict(n: Int): P

    /**
     * Push a constant into the frame
     * @param c constant to push into the frame
     */
    def evalConstant(c: Double): P

    /**
     * Push a constant into the frame
     * @param c constant to push into the frame
     */
    def evalConstant(c: String): P

    /**
     * Push a null constant into the frame
     */
    def evalNull(tpe: soot.Type = NullType.v()): P

    /**
     * Push a new object into the frame
     * @param tpe the type of the new object
     */
    def evalNew(tpe: soot.Type): P

    /**
     * Add a new variable of unknown value
     * @param t the type of the new variable
     */
    def evalUnknown(tpe: soot.Type): Property

    /**
     * Evaluate  a frame variable `i` and push a copy into the frame
     * @param i the frame variable to evaluate
     */
    def evalLocal(i: Int): P

    /**
     * Evaluate a field of a frame variable, and push a copy into the frame
     * @param i the frame variable to evaluate
     * @param f the field to evaluate within `i`
     */
    def evalField(i: Int = dimension - 1, f: soot.SootField): P

    /**
     * Cast the top element of the frame to the new type
     * @param t the new type
     */
    def evalCast(t: soot.Type): Property

    /**
     * Sums the two top element of the frame and replace them with the result
     */
    def evalAdd: P

    /**
     * Subtracts the top element of the frame from the element `size`-2 and replace them with the result.
     */
    def evalSub: P

    /**
     * Multiplies the two top element of the frame and replace them with the result.
     */
    def evalMul: P

    /**
     * Divides the element `size`-2 with the top element of the frame and replace both of them with the result.
     */
    def evalDiv: P

    /**
     * Sum an integer constant to the frame variable 'i' and replace the latter with the result
     */
    def evalInc(i: Int = dimension - 1, c: Double): P = evalLocal(i).evalConstant(c).evalAdd.assignLocal(i)

    /**
     * Computes the reminder of dividing the element `size`-2 with the top element, and replace them with the result.
     */
    def evalRem: P

    /**
     * Computes el(size-2) << el(size-1) and replace the two top elements of the stack with the result.
     */
    def evalShl: P

    /**
     * Computes el(size-2) >> el(size-1) and replace the two top elements of the stack with the result.
     */
    def evalShr: P

    /**
     * Computes el(size-2) >>> el(size-1) and replace the two top elements of the stack with the result.
     */
    def evalUshr: P

    /**
     * Computes the effect of a non-specified operations between el(size-2) and el(size-1), and replace
     * the two top elements of the stack with the result.
     */
    def evalBinOp: P

    /**
     * Replace the top of the stack with its opposite.
     */
    def evalNeg: P

    /**
     * Replace the top of the stack (which should be an array object) with its length.
     */
    def evalLength: P

    /**
     * Evaluates the boolean expression el(size-2) > el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalGt: P

    /**
     * Evaluates the boolean expression el(size-2) >= el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalGe: P

    /**
     * Evaluates the boolean expression el(size-2) < el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalLt: P

    /**
     * Evaluates the boolean expression el(size-2) <= el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalLe: P

    /**
     * Evaluates the boolean expression el(size-2) == el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalEq: P

    /**
     * Evaluates the boolean expression el(size-2) != el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalNe: P

    /**
     * Assign to the variable frame `i` the value at the top of the frame, and pop it.
     * @param i the frame variable to assign
     */
    def assignLocal(i: Int): P

    /**
     * Assign to  a field of the variable frame `i` the value at the top of the frame, and pop it.
     * @param i the frame variable to assign
     * @param f the field to assign
     */
    def assignField(i: Int = dimension - 2, f: soot.SootField): P

    /**
     * Assign to a static field of the variable frame `i` the value at the top of the frame, and pop it.
     * @param i the frame variable to assign
     * @param f the field to assign
     */
    def assignStaticField(i: Int = dimension - 2, f: soot.SootField): P

    /**
     * Returns the result of testing whether el(size-2) > el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testGt: (P, P)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testGe: (P, P)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testLt: (P, P)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testLe: (P, P)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testEq: (P, P)

    /**
     * Returns the result of testing whether el(size-2) >= el(size-1) and pops the two top frame
     * positions. The first element is the abstract frame for the "then"-branch, the second is for
     * the "else"-branch.
     */
    def testNe: (P, P)

    /**
     * Returns the result of testing the frame w.r.t. a linear condition. It does not pop the stack.
     * @param lf the linear condition to use for testing.
     */
    def testLinearCondition(lf: NumericCondition): (P, P)

    /**
     * Evaluates a global constant and put the result on the frame.
     * @param o the constant to evaluate.
     */
    def evalGlobal(o: soot.jimple.Constant): P

    /**
     * Evaluate a static field and put the result on the top of the frame.
     * @param v the static field to evaluate.
     */
    def evalStaticField(v: soot.SootField): P

    /**
     * Exchange the position of the top element of the frame with that situated in the position i.
     */
    def evalSwap(i: Int = dimension - 2, j: Int = dimension - 1): P

    /**
     * Create a duplicate of the top element of the frame and insert it on the top of the frame
     */
    def evalDup1(): P = evalLocal(dimension - 1)

    /**
     * Create a duplicate of the top element of the frame and insert it two positions under the top of the frame
     */
    def evalDup1_x1(): P = evalSwap().evalLocal(dimension - 2)

    /**
     * Create a duplicate of the top element of the frame and insert it three positions under the top of the frame
     */
    def evalDup1_x2(): P = evalSwap(dimension - 3, dimension - 1).evalSwap().evalLocal(dimension - 3)

    /**
     * Create two duplicate of the two top element of the frame and insert them on the top of the frame
     */
    def evalDup2(): P = evalLocal(dimension - 2).evalLocal(dimension - 1)

    /**
     * Create two duplicate of the two top element of the frame and insert them three positions under the top of the frame
     */
    def evalDup2_x1(): P = evalSwap(dimension - 3, dimension - 2).evalSwap().evalLocal(dimension - 3).evalLocal(dimension - 3)

    /**
     * Create two duplicate of the two top element of the frame and insert them four positions under the top of the frame
     */
    def evalDup2_x2(): P = evalSwap(dimension - 4, dimension - 2).evalSwap(dimension - 3, dimension - 1).evalLocal(dimension - 4).evalLocal(dimension - 4)

    /**
     * Evaluates the result of a cast of the top of stack to type `t`
     * @param t the type in which to cast the top of the stack
     */
    def evalInstance(t: soot.Type): P

    /**
     * Evaluates the effect of entering a monitor.
     * @param n the frame variable whose monitor is entered.
     * @todo really do something instead of returning this
     */
    def enterMonitor(n: Int = dimension - 1): P = this

    /**
     * Evaluates the effect of exiting a monitor.
     * @param n the frame variable whose monitor is exited.
     */
    def exitMonitor(n: Int = dimension - 1): P = this

    /**
     * The connect method is used for inter-procedural analysis. It takes two properties
     * such that the last `common` dimensions of `this` corresponds to the first `common`
     * dimension of `other`. The first represents the abstract state before calling a
     * procedure, the second represents the abstract state at the end of the procedure.
     * `connect` merge the two abstract states using a call-by-value semantics, and
     * remove the common dimension.
     * @todo why not remove the private dimensions before connecting?
     * @todo we should better understand the concrete semantic of this operation
     */
    def connect(other: Property, common: Int): Property
  }
}
