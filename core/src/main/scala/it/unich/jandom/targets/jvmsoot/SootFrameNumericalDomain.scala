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
 * but WITHOUT ANY WARRANTY; without even the implied warranty ofa
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
import it.unich.jandom.domains.numerical.LinearForm
import it.unich.jandom.domains.numerical.NumericalDomain
import it.unich.jandom.targets.NumericCondition
import it.unich.jandom.targets.NumericCondition._
import it.unich.scalafix.Box
import soot._
import soot.baf.DoubleWordType
import soot.baf.WordType
import soot.jimple.Constant
import spire.math.Rational

/**
 * This class implements an abstract frame for Soot where only numerical variables are considered.
 * @param numdom the numerical domain to use for representing properties of numerical variables.
 * @author Gianluca Amato <gianluca.amato@unich.it>
 * @author Luca Mangifesta
 */
class SootFrameNumericalDomain(val numdom: NumericalDomain) extends SootFrameDomain {

  def top(types: Seq[Type]) = Property(numdom.top(types.size), List(types.reverse: _*))

  def bottom(types: Seq[Type]) = Property(numdom.bottom(types.size), List(types.reverse: _*))

  val widenings = for (w <- numdom.widenings) yield WideningDescription(w.name, w.description,
    Box { (a: Property, b: Property) => Property(w(a.prop, b.prop), a.stack) })

  /**
   * A simple helper method for the analogous constructor of abstract numerical frames.
   * @param num
   */
  def apply(prop: numdom.Property, types: Seq[Type]) = new Property(prop, List(types.reverse: _*))

  /**
   * A simple helper method for the analogous constructor of abstract numerical frames.
   */
  def apply(prop: numdom.Property, tpe: Type) = new Property(prop, tpe)

  /**
   * Determines whether `t` is a numeric type
   */
  def isNumeric(t: Type) = t.isInstanceOf[PrimType] || t.isInstanceOf[WordType] || t.isInstanceOf[DoubleWordType]

  /**
   * This class represents a single abstract frame.
   * @param prop contains the numeric property giving informations on numerical variables. Dimensions are allocated also for
   * non-numerical variables, but their value is undetermined
   * @param vars the stack of variable types in the current frame. Note that stack position are numbered in the
   * opposite way than frame variables, i.e., the frame variable `i` corresponds to stack position `size - 1 - i`.
   */
  case class Property(val prop: numdom.Property, val stack: List[Type]) extends SootFrameProperty[Property] {

    invariantCheck

    /**
     * This method check invariants on a numerical abstract frame.
     */
    @elidable(ASSERTION)
    private def invariantCheck() {
      assert(prop.dimension == stack.size, "Numerical property and stack have different dimensions")
      if (!prop.isEmpty)
        for (i <- 0 until prop.dimension) stack(dimension - 1 - i) match {
          case _: PrimType | _: WordType | _: DoubleWordType =>
          case _ =>
            // TODO: I should check that dimension i is unconstrained. For now I check that it is unbounded
            assert(prop.minimize(LinearForm.v(i)).isNegInfinity, "A non-numerical variable should be unconstrained")
            assert(prop.maximize(LinearForm.v(i)).isPosInfinity, "A non-numerical variable should be unconstrained")
        }
    }

    /**
     * An alternative constructor which returns an abstract frame where all variables are of the same type.
     * @param prop the numerical property to use.
     * @param tpe the common type of all frame variables.
     */
    def this(prop: numdom.Property, tpe: Type) = {
      this(prop, List.fill(prop.dimension)(tpe))
    }

    type Domain = SootFrameNumericalDomain.this.type

    def domain = SootFrameNumericalDomain.this

    def tryCompareTo[B >: Property](other: B)(implicit arg0: (B) => PartiallyOrdered[B]): Option[Int] =
      other match {
        case other: Property =>
          assume(other.stack == stack, "The abstract frame have different variables")
          prop tryCompareTo other.prop
        case _ => None
      }

    def top = domain(prop.top, stack)

    def bottom = domain(prop.bottom, stack)

    def isTop = prop.isTop

    def isBottom = prop.isBottom

    def isEmpty = prop.isEmpty

    def union(that: Property) = Property(prop union that.prop, stack)

    def intersection(that: Property) = Property(prop intersection that.prop, stack)

    def widening(that: Property) = Property(prop widening that.prop, stack)

    def narrowing(that: Property) = Property(prop narrowing that.prop, stack)

    def mkString(vars: Seq[String]) = prop.mkString(vars) + "types: " + this.stack.reverse.mkString(",")

    def fiber = stack

    def dimension = prop.dimension

    def addVariable(tpe: Type) = Property(prop.addVariable(), tpe :: stack)

    def delVariable(m: Int) = Property(prop.delVariable(m), stack.take(dimension - 1 - m) ++ stack.takeRight(m))

    def mapVariables(rho: Seq[Int]) = {
      val newstack = for (i <- rho; if i != -1) yield stack(dimension - 1 - i)
      Property(prop.mapVariables(rho), List(newstack.reverse: _*))
    }

    def connect(p: Property, common: Int) = {
      assume((stack.dropRight(dimension - common) zip p.stack.drop(p.dimension - common)) forall { case (tdst, tsrc) => compatibleTypes(tdst, tsrc) })
      Property(prop.connect(p.prop, common), p.stack.dropRight(common) ++ stack.drop(common))
    }

    def extract(n: Int) = {
      assume(n >= 0 && n <= dimension, s"Trying to extract ${n} variables in the abstract frame {$this}")
      Property(prop.delVariables(0 until dimension - n), stack.dropRight(dimension - n))
    }

    def restrict(n: Int) = {
      assume(n >= 0 && n <= dimension, s"Trying to restrict ${n} variables top in the abstract frame {$this}")
      Property(prop.delVariables(dimension - n until dimension), stack.drop(n))
    }

    def evalConstant(const: Double) = Property(prop.addVariable().constantAssignment(dimension, const), DoubleType.v :: stack)

    def evalCast(t: soot.Type) = Property(this.prop, t :: stack.tail)

    def evalConstant(const: String) = Property(prop.addVariable(), RefType.v(const.getClass().getName()) :: stack)

    def evalNull(tpe: Type = soot.NullType.v()) = addVariable(tpe)

    def evalNew(tpe: Type) = addVariable(tpe)

    def evalUnknown(tpe: Type) = addVariable(tpe)

    def evalLocal(v: Int) = {
      if (isNumeric(stack(dimension - 1 - v)))
        Property(prop.addVariable().variableAssignment(dimension, v), stack(dimension - 1 - v) :: stack)
      else
        addVariable(stack(dimension - 1 - v))
    }

    def evalField(v: Int, f: SootField) = {
      assume(stack(dimension - 1 - v).isInstanceOf[RefType], "Expected RefType, got " + stack(dimension - 1 - v))
      addVariable(f.getType())
    }

    def evalStaticField(v: SootField): Property = addVariable(v.getType())

    def evalGlobal(o: Constant): Property = addVariable(o.getType())

    def assignLocal(dst: Int) = {
      if (isNumeric(stack(dimension - 1 - dst)))
        Property(prop.variableAssignment(dst, dimension - 1).delVariable(), stack.tail)
      else
        delVariable()
    }

    def assignField(dst: Int, f: SootField) = {
      assume(stack(dimension - 1 - dst).isInstanceOf[RefType], "Expected RefType, got " + stack(dimension - 1 - dst))
      delVariable()
    }

    def assignStaticField(dst: Int, f: SootField) = {
      delVariable()
    }

    def evalInstance(t: Type) = {
      prop.nonDeterministicAssignment(dimension - 1)
      delVariable()
    }

    def evalAdd = Property(prop.variableAdd().delVariable(), stack.tail)

    def evalSub = Property(prop.variableSub().delVariable(), stack.tail)

    def evalMul = Property(prop.variableMul().delVariable(), stack.tail)

    def evalDiv = Property(prop.variableDiv().delVariable(), stack.tail)

    def evalRem = Property(prop.variableRem().delVariable(), stack.tail)

    def evalShl = Property(prop.variableShl().delVariable(), stack.tail)

    def evalShr = Property(prop.variableShr().delVariable(), stack.tail)

    def evalUshr = Property(prop.variableUshr().delVariable(), stack.tail)

    def evalBinOp = Property(prop.nonDeterministicAssignment(dimension - 2).delVariable(), stack.tail)

    def evalNeg = Property(prop.variableNeg(), stack)

    def evalLength = delVariable().addVariable(IntType.v())

    def evalGt = delVariable()

    def evalGe = delVariable()

    def evalLt = delVariable()

    def evalLe = delVariable()

    def evalEq = delVariable()

    def evalNe = delVariable()

    /**
     * A generic test operation which depends on a comparison operator.
     * @param op the comparison operator to use
     */
    private def testComp(op: ComparisonOperators.Value) = {
      val lf = LinearForm.sparse(Rational.zero, dimension - 2 -> Rational.one, dimension - 1 -> -Rational.one)
      val tbranch = Property(AtomicCond(lf, op).analyze(prop).delVariable().delVariable(), stack.tail.tail)
      val fbranch = Property(AtomicCond(lf, ComparisonOperators.opposite(op)).analyze(prop).delVariable().delVariable(), stack.tail.tail)
      (tbranch, fbranch)
    }

    def testGt = testComp(ComparisonOperators.GT)

    def testGe = testComp(ComparisonOperators.GTE)

    def testLe = testComp(ComparisonOperators.LTE)

    def testLt = testComp(ComparisonOperators.LT)

    def testEq = testComp(ComparisonOperators.EQ)

    def testNe = testComp(ComparisonOperators.NEQ)

    def testLinearCondition(lc: NumericCondition) = (
      Property(lc.analyze(prop), stack), Property(lc.opposite.analyze(prop), stack))

    def evalSwap(i: Int, j: Int): Property = {
      val seq = ((0 until i - 1) :+ j) ++ ((i + 1 until j - 1) :+ i) ++ (j + 1 until dimension - 1)
      val newstack = stack updated (i, stack.apply(j)) updated (j, stack.apply(i))
      Property(prop.mapVariables(seq), newstack)
    }

  }
}
