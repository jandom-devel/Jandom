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

import it.unich.sci.jandom.targets.linearcondition.LinearCond
import it.unich.sci.jandom.domains.AbstractDomain
import it.unich.sci.jandom.domains.AbstractProperty
import it.unich.sci.jandom.domains.CartesianFiberedProperty
import it.unich.sci.jandom.domains.CartesianFiberedDomain

/**
 * This is the base trait domain for the analysis of methods using Soot. It represents
 * an abstract typed frame with locals and stack. It is used for both Baf and Jimple analysis,
 * although Jimple could be made faster by analyzing one statement at a time instead of
 * evaluating expressions compositionally.
 * @author Gianluca Amato <gamato@unich.it>
 * @author Luca Mangifesta
 */
trait SootFrameDomain extends CartesianFiberedDomain {

  type FiberType = soot.Type

  type Property <: SootFrameProperty[Property]

  /**
   * Returns the top abstract property relative to the fiber `vars`
   */
  def top(vars: Seq[FiberType]): Property

  /**
   * Returns the bottom abstract property relative to the fiber `vars`
   */
  def bottom(vars: Seq[FiberType]): Property

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
  trait SootFrameProperty[Property <: SootFrameProperty[Property]] extends CartesianFiberedProperty[soot.Type, Property] {
    this: Property =>

    /**
     * Returns the last `n` dimensions.
     */
    def extract(n: Int): Property

    /**
     * Deletes the last `n` dimensions
     */
    def restrict(n: Int): Property

    /**
     * Push a constant into the frame
     * @param c constant to push into the frame
     */
    def evalConstant(c: Double): Property

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
    def evalNew(tpe: soot.Type): Property

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
    def evalField(i: Int = dimension - 1, f: soot.SootField): Property

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
     * Sum an integer constant to the frame variable 'i' and replace the latter with the result
     */
    def evalInc(i: Int = dimension - 1, c: Double): Property = evalLocal(i).evalConstant(c).evalAdd.assignLocal(i)

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
     * Evaluates the boolean expression el(size-2) > el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalGt: Property

    /**
     * Evaluates the boolean expression el(size-2) >= el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalGe: Property

    /**
     * Evaluates the boolean expression el(size-2) < el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalLt: Property

    /**
     * Evaluates the boolean expression el(size-2) <= el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalLe: Property

    /**
     * Evaluates the boolean expression el(size-2) == el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalEq: Property

    /**
     * Evaluates the boolean expression el(size-2) != el(size-1), and replace the two
     * top elements of the stack with the result.
     */
    def evalNe: Property

    /**
     * Assign to the variable frame `i` the value at the top of the frame, and pop it.
     * @param i the frame variable to assign
     */
    def assignLocal(i: Int): Property

    /**
     * Assign to  a field of the variable frame `i` the value at the top of the frame, and pop it.
     * @param i the frame variable to assign
     * @param f the field to assign
     */
    def assignField(i: Int = dimension - 2, f: soot.SootField): Property

    /**
     * Assign to a static field of the variable frame `i` the value at the top of the frame, and pop it.
     * @param i the frame variable to assign
     * @param f the field to assign
     */
    def assignStaticField(i: Int = dimension - 2, f: soot.SootField): Property

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
     * Returns the result of testing the frame w.r.t. a linear condition. It does not pop the stack.
     * @param lf the linear condition to use for testing.
     */
    def testLinearCondition(lf: LinearCond): (Property, Property)

    /**
     * Evaluates a global constant and put the result on the frame.
     * @param o the constant to evaluate.
     */
    def evalGlobal(o: soot.jimple.Constant): Property

    /**
     * Evaluate a static field and put the result on the top of the frame.
     * @param v the static field to evaluate.
     */
    def evalStaticField(v: soot.SootField): Property

    /**
     * Exchange the position of the top element of the frame with that situated in the position i.
     */
    def evalSwap(i: Int = dimension - 2, j: Int = dimension - 1): Property

    /**
     * Create a duplicate of the top element of the frame and insert it on the top of the frame
     */
    def evalDup1(): Property = evalLocal(dimension - 1)

    /**
     * Create a duplicate of the top element of the frame and insert it two positions under the top of the frame
     */
    def evalDup1_x1(): Property = evalSwap().evalLocal(dimension - 2)

    /**
     * Create a duplicate of the top element of the frame and insert it three positions under the top of the frame
     */
    def evalDup1_x2(): Property = evalSwap(dimension - 3, dimension - 1).evalSwap().evalLocal(dimension - 3)

    /**
     * Create two duplicate of the two top element of the frame and insert them on the top of the frame
     */
    def evalDup2(): Property = evalLocal(dimension - 2).evalLocal(dimension - 1)

    /**
     * Create two duplicate of the two top element of the frame and insert them three positions under the top of the frame
     */
    def evalDup2_x1(): Property = evalSwap(dimension - 3, dimension - 2).evalSwap().evalLocal(dimension - 3).evalLocal(dimension - 3)

    /**
     * Create two duplicate of the two top element of the frame and insert them four positions under the top of the frame
     */
    def evalDup2_x2(): Property = evalSwap(dimension - 4, dimension - 2).evalSwap(dimension - 3, dimension - 1).evalLocal(dimension - 4).evalLocal(dimension - 4)

    /**
     * Evaluates the result of a cast of the top of stack to type `t`
     * @param t the type in which to cast the top of the stack
     */
    def evalInstance(t: soot.Type): Property

    /**
     * Evaluates the effect of entering a monitor.
     * @param n the frame variable whose monitor is entered.
     * @todo really do something instead of returning this
     */
    def enterMonitor(n: Int = dimension - 1): Property = this

    /**
     * Evaluates the effect of exiting a monitor.
     * @param n the frame variable whose monitor is exited.
     */
    def exitMonitor(n: Int = dimension - 1): Property = this
  }
}
